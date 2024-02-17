

#...............................................................................
#                                                                              .
#  Yadvinder suggested to calculate a GPP based on field measured Vcmax Jmax   .
#  and ci/ca, this script try to plug Jmax Vcmax and ci/ca into rpmodel        .
#                                                                              .
#...............................................................................

# we can add one more simulation to pmodel to replace light use efficiency with the light use efficiency deploy in MODIS to see the effect of using optimality.
# Light use effeicnecy in modis are via a lookup table:
# we get 0.078 from Table A1 of this paper: https://www.sciencedirect.com/science/article/pii/S0034425716302164?casa_token=ULzLp0reS4IAAAAA:uir8Mpr1MDTX6Zq4Dlyci5Oh6OqsB9P1L9butDY4tYPpDxi9L4fOeBsP9PI-DHh-WqKB55gn#s0115
# Supplementary of Zhao and running science Table S1 (note that the unit here is wrong, should be (KgC/MJ)
# to convert unit, we have 1 W/m2 = 0.0864 MJ/m2/day, light use efficiency is  0.078/0.0864/1000 = 0.0009027778 Kgc/MJ
# you should also check this for light use efficiency which is Emax: https://www.mdpi.com/2072-4292/6/4/3321#b20-remotesensing-06-03321


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- FIELD CLIMATE + FIELD TRAIT---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




library(multcompView)
library(tidyverse)
library(Hmisc)
library(multcomp)
library(vegan)
library(ARTofR)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggsci)
library(ggpubr)
rm(list=ls())
setwd("F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/")
source('F:/Oxford/Chapter_three/github_version/Rpmodel_V3/subroutines.R')
Field_measurement_table<-openxlsx::read.xlsx('F:/Oxford/Chapter_three/github_version/3.supplementary_compare_climate_variable/Field_measurement_photosynthesis_trait.xlsx' )

Ratio_Jmax_to_vcmax <- Field_measurement_table$Jmax_onepoint_25C / Field_measurement_table$Vcmax_onepoint_25C

# OMG there are all around the typical values 1.88!!!


library(tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ prepare for p model     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




J_to_mol = 4.6 # this will convert J m-2 s-1 (= W m-2) to umol (quanta) m-2 s-1
to_PPFD=1*1000000000/365/24/3600 * J_to_mol / 1000000 *3600*24*0.45 # this one will convert  (GJ m-2 year-1) to mol/m2/d, the 0.45 is photosynthetically active fraction (which is called PAR, see Wang et al Nature plants)
c_molmass <- 12.0107  # molecular mass of carbon (g)

kPo <- 101325.0       # standard atmosphere, Pa (Allen, 1973)
kTo <- 25.0           # base temperature, deg C (Prentice, unpublished)
rd_to_vcmax <- 0.015  # Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
soilmstress <- 1.0


beta = 146.0
load("F:/Oxford/Chapter_three/github_version/2_Figure_four_timeseries_of_fapar/organized_climate_withppfd_two_fapar.rda")
result$VPDmean=result$VPDmean*1000
result$FAPAR=result$fapar_no_cloud/100
result$ppfd=result$ppfd/30 # to mol/m2/d,

#result$grow_temp=c(26.53057,25.50788,27.09553) #These are temperature calculatd by 20200909_growth_temp_code.R
#result$grow_temp=c(25.7,25,26.4) #These are temperature in Moore et al 2018


colnames(result)[1]<-'SITE'

#  result$ppfd[result$SITE=='ANK'] <- 1.7*to_PPFD # These values are from Sam Moore's paper,
#  result$ppfd[result$SITE=='BOB'] <- 2.0*to_PPFD
#  result$ppfd[result$SITE=='KOG'] <- 2.4*to_PPFD
# To use the field measured ppfd as in Moore et al 2018 GCB


List_of_site<-c('KOG','BOB','ANK')
cls <- data.frame(SITE=List_of_site, colour=c("#deebf7","#9ecae1","#3182bd"))
result <- result%>%
    left_join(cls, by="SITE")



 result$SITE <- factor(result$SITE, levels = c("ANK", "BOB", "KOG"))
result<-arrange(result,SITE)


Field_measurement_table<-left_join(Field_measurement_table,result, by='SITE')%>%
  dplyr::rename(chi  = cica,
                tc = Tmean)

result$SITE <- str_replace(result$SITE, "ANK", "ANK (wet)")
result$SITE <- str_replace(result$SITE, "BOB", "BOB (mid)")
result$SITE <- str_replace(result$SITE, "KOG", "KOG (dry)")
Field_measurement_table$SITE <- factor(Field_measurement_table$SITE, levels = c("ANK", "BOB", "KOG"))
Field_measurement_table$SITE <- str_replace(Field_measurement_table$SITE, "ANK", "ANK (wet)")
Field_measurement_table$SITE <- str_replace(Field_measurement_table$SITE, "BOB", "BOB (mid)")
Field_measurement_table$SITE <- str_replace(Field_measurement_table$SITE, "KOG", "KOG (dry)")

Field_measurement_table<-Field_measurement_table%>%
  mutate(jmax=Jmax_onepoint_25C*jmax25_to_jmax ,
         vcmax=Vcmax_onepoint_25C*vcmax25_to_vcmax,
         jmax_se=Jmax_onepoint_se*jmax25_to_jmax,
         vcmax_se = Vcmax_onepoint_se*vcmax25_to_vcmax)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Field CLIMATE and Field TRAIT                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ run-pmodel with field condition  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Field_measurement_based_GPP<-Field_measurement_table%>%
  mutate(
    jmax = Jmax_onepoint_25C      * jmax25_to_jmax / (10^6 /24/3600),
    vcmax = Vcmax_onepoint_25C  * vcmax25_to_vcmax / (10^6 /24/3600), # mol C m-2 day-1 
    patm = calc_patm(elv),
    co2 = 414,
    # kphio=0.081785*ftemp_kphio( tc, c4=FALSE ), # this is from GMD Stocker 2020
    #kphio=1.02 / c_molmass, # this value is from Wang Han Nature plants 
    #kphio=0.257/4, # from this paper https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13210, or this one: https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15276?casa_token=dfH1_JTf6RUAAAAA%3A2hC3HOjjeogdfS5LGgdrekV3SHO8zCsbMN4Qc0qg0QdoBXt0Y-NNke3JIIAhKpqFCvv4TMLkbR3oOA
    kphio = (0.352+0.021*tc-tc^2 * 3.4/10^4)/8,  # this is from Peng et al New phytologist 2020 
    ca = co2_to_ca( co2, patm ),
   ## photorespiratory compensation point - Gamma-star (Pa)
   gammastar = calc_gammastar( tc, patm ),
   ## Michaelis-Menten coef. (Pa)
   kmm = calc_kmm( tc, patm ),   ## XXX Todo: replace 'NA' here with 'patm'
   ## viscosity correction factor = viscosity( temp, press )/viscosity( 25 degC, 1013.25 Pa)
   ns      = viscosity_h2o( tc, patm ),  # Pa s
   ns25    = viscosity_h2o( kTo, kPo ),  # Pa s
   ns_star = ns / ns25,  # (unitless)
   
   #................This is the most important line.................
   xi  = sqrt( (beta * ( kmm + gammastar ) ) / ( 1.6 * ns_star ) ),
   chi = gammastar / ca + ( 1.0 - gammastar / ca ) * xi / ( xi + sqrt(VPDmean) ),
   ci = chi * ca,
   gamma = gammastar / ca,
   kappa = kmm / ca,
   
   ## use chi for calculating mj
   mj = (chi - gamma) / (chi + 2 * gamma),
   
   ## mc
   mc = (chi - gamma) / (chi + kappa),
   
   ## mj:mv
   mjoc = (chi + kappa) / (chi + 2 * gamma), 
   a_c = vcmax * (ci - gammastar) / (ci + kmm),
   fact_jmaxlim = 1/(sqrt(1+(4*kphio*ppfd_field /jmax)^2)),
   a_j = kphio * ppfd_field  * (ci - gammastar)/(ci + 2 * gammastar) * fact_jmaxlim, # unit is mol C /m2 /day 
   LUE = a_j * c_molmass / (ppfd_field /4.6) , # ppfd is in unit (mol /m2 / day), use 4.6 to convert to MJ/m2/day, now LUE is in gC/MJ
   a_coordinated=(a_j+a_j)/2, # Colin suggested to follow A_j only after checking coordination hypothesis
   gpp_calculated = a_coordinated *FAPAR_field*  c_molmass / 10^6 * 10000 * 365 
  )
Field_measurement_based_GPP$vcmax<-Field_measurement_based_GPP$vcmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
Field_measurement_based_GPP$jmax<-Field_measurement_based_GPP$jmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
#openxlsx::write.xlsx(Field_measurement_based_GPP,file = paste0(Sys.Date(),'_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx' ))
Field_measurement_based_GPP<-openxlsx::read.xlsx( '2023-04-14_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')


Field_measurement_based_GPP$SITE<-c('ANK','BOB','KOG')



GPP_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=gpp), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=gpp, ymin=gpp-gpp_se, ymax=gpp+gpp_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_based_GPP)+ 
  geom_point(aes(x=SITE, y=gpp_calculated),color='red', size=2.4,data = Field_measurement_based_GPP)+
  ylab(bquote('GPP (MgC'~ ha^-1~year^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_based_GPP$gpp,na.rm=T)*0,max(Field_measurement_based_GPP$gpp,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

jmax_figure<-ggplot(Field_measurement_based_GPP) +
  geom_bar( aes(x=SITE, y=jmax), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=jmax, ymin=jmax-jmax_se, ymax=jmax+jmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_based_GPP)+ 
  geom_point(aes(x=SITE, y=jmax),color='red', size=2.4,data = Field_measurement_based_GPP)+  ylab(bquote('Jmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_based_GPP$jmax,na.rm=T)*0,max(Field_measurement_based_GPP$jmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

vcmax_figure<-ggplot(Field_measurement_based_GPP) +
  geom_bar( aes(x=SITE, y=vcmax), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=vcmax, ymin=vcmax-vcmax_se, ymax=vcmax+vcmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_based_GPP)+ 
  geom_point(aes(x=SITE, y=vcmax),color='red', size=2.4,data = Field_measurement_based_GPP)+  ylab(bquote('Vcmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_based_GPP$vcmax,na.rm=T)*0,max(Field_measurement_based_GPP$vcmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

plot0<- ggarrange(GPP_figure, jmax_figure, vcmax_figure, ncol = 3, nrow = 1)
annotate_figure(plot0, top = text_grob("Experiment Pmodel_PfT\n (Using field based PPFD fAPAR and LUE)", 
                                      color = "black", face = "bold", size = 10))

# / (10^6 /24/3600)  this part change from  umol m-2 s-1 to mol C m-2 day-1  (the later one is the common unit in rpmodel)
# c_molmass is the mole mass of carbon
# / 10^6 * 10000 * 365 this part correct the unit of gpp from g C m-2 day-1 to MgC per ha yr-1
#ggsave(filename = "F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_trait.jpg",width = 4.57, height = 2.7, dpi = 300)


ac_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=a_c ), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  ylab(bquote(A[C](mol~C ~ m^-2~day^-1)))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(0,1.2)) +  theme(title = element_text(size=9),
        plot.title = element_markdown()) 
aj_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=a_j ), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  ylab(bquote(A[J](mol~C ~ m^-2~day^-1)))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(0,1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 
plot02<- ggarrange(ac_figure, aj_figure, ncol = 3, nrow = 1)+ggtitle('Calculate GPP with measured traits and measured climate')
annotate_figure(plot02, top = text_grob("Experiment Pmodel_PfT\n (Checking coordination hypothesis Ac=Aj)", 
                                       color = "black", face = "bold", size = 10))
#ggsave(filename = "F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_trait_ac_aj.jpg",width = 4.57, height = 2.7, dpi = 300)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        fixed LUE to Zhao and running                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ run-pmodel with field condition  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I tried to calculate f_VPD and f_Tair following user guide of MOD17A2  and https://www.mdpi.com/2072-4292/6/4/3321#b8-remotesensing-06-03321
# however, it ends up with very very small LUE, don't know why....I must have made something wrong,

pizzass<-read.csv("H:/Oxford/Chapter_one/From_imma/climate_data/BOB_gapfill_erai_daily_1979_2017.csv")%>%
  filter(year>2013 & year<2015)%>%
  filter(VPDmean>0)
VPD_maxmax=max(pizzass$VPDmean)
VPD_dd=max(pizzass$VPDmean)-min(pizzass$VPDmean)
Tminmin= min(pizzass$Tmin)
Tminmax = max(pizzass$Tmin)
pizzass<-pizzass%>%
  mutate(
  f_VPD = (VPD_maxmax- VPDmean ) / VPD_dd,
  f_Tair = (Tmin -Tminmin)/(Tminmax - Tminmin),
  scalar = f_Tair * f_VPD
)
mean(pizzass$scalar)


GPP_fixed_LUE<-Field_measurement_table%>%
  mutate(

    LUE = 0.001165*1000*mean(pizzass$scalar), # DBF plant functional type
    a_j = LUE / c_molmass * (ppfd_field/4.6),

    gpp_calculated = LUE  * (ppfd_field/4.6 ) *FAPAR  / 10^6 * 10000 * 365 
  )

openxlsx::write.xlsx(GPP_fixed_LUE,file = paste0(Sys.Date(),'_fixed_LUE_p-model_output.xlsx' ))
Field_measurement_based_GPP<-openxlsx::read.xlsx( '2022-12-12_fixed_LUE_p-model_output.xlsx')



Field_measurement_based_GPP$vcmax<-Field_measurement_based_GPP$vcmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
Field_measurement_based_GPP$jmax<-Field_measurement_based_GPP$jmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1


Field_measurement_based_GPP$gpp_se<-Field_measurement_based_GPP$gpp_se
GPP_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=gpp), stat="identity",data=Field_measurement_based_GPP, fill=Field_measurement_based_GPP$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=gpp, ymin=gpp-gpp_se, ymax=gpp+gpp_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_based_GPP)+ 
  geom_point(aes(x=SITE, y=gpp_calculated),color='red', size=2.4,data = Field_measurement_based_GPP)+
  ylab("GPP (MgC ha yr-1)")+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_based_GPP$gpp,na.rm=T)*0,max(Field_measurement_based_GPP$gpp,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- model fapar + model TRAIT---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("F:/Oxford/Chapter_three/github_version/Rpmodel_V3")
files.sources = list.files(pattern ='*.R')
sapply(files.sources, source)
      
setwd("F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/")

for (i in 1:3){
  temp_result<-Field_measurement_table[i,]
  rp_model_output<-rpmodel(
    tc=temp_result$tc,
    vpd=temp_result$VPDmean,
    co2=414,
    fapar=temp_result$FAPAR,
    ppfd=temp_result$ppfd_field ,
    patm = NA,
    elv=temp_result$elv,
    beta = 146,
    soilm = 1,
    meanalpha = 1,
    apar_soilm = 0,
    bpar_soilm = 0.733,
    c4 = FALSE,

    method_jmaxlim = "wang17", #Other options are "smith19" or 'none' "wang17",
    do_ftemp_kphio = TRUE,
    do_soilmstress = FALSE,
    returnvar = NULL,
    verbose = FALSE
  )
  if (i==1) {
    rp_model_output2<-as.data.frame(rp_model_output)
  } else {
    rp_model_output2[i,]<-as.data.frame(rp_model_output)
  }
  rp_model_output2$SITE[i]<-as.character(result[i,'SITE'])
}


result_no_soilm<-dplyr::left_join(result,rp_model_output2,by="SITE")
result_no_soilm$vcmax<-result_no_soilm$vcmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$vcmax25<-result_no_soilm$vcmax25 * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax<-result_no_soilm$jmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax25<-result_no_soilm$jmax25* 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$gpp<-result_no_soilm$gpp / 10^6 * 10000 * 365 #from g C m-2 day-1 to MgC per ha yr-1
result_no_soilm<-result_no_soilm%>%
  mutate(vcmax=vcmax/FAPAR,
         vcmax25 = vcmax25/FAPAR,
         jmax = jmax/FAPAR,
         jmax25 = jmax25/FAPAR,
         iabs_no_fpar=iabs/FAPAR)%>%
  dplyr::rename(iabs_with_fpar=iabs)




result_no_soilm$SITE<-c('ANK','BOB','KOG')
Field_measurement_table$SITE<-c('ANK','BOB','KOG')

GPP_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=gpp), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=gpp, ymin=gpp-gpp_se, ymax=gpp+gpp_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=gpp),color='red', size=2,data = result_no_soilm)+
  ylab(bquote('GPP (MgC'~ ha^-1~year^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilm$gpp,na.rm=T)*0,max(Field_measurement_table$gpp,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

jmax_figure<-ggplot(result_no_soilm) +
  geom_bar( aes(x=SITE, y=jmax), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=jmax, ymin=jmax-jmax_se, ymax=jmax+jmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=jmax),color='red', size=2,data = result_no_soilm)+   ylab(bquote('Jmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_table$jmax,na.rm=T)*0,max(Field_measurement_table$jmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

vcmax_figure<-ggplot(result_no_soilm) +
  geom_bar( aes(x=SITE, y=vcmax), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=vcmax, ymin=vcmax-vcmax_se, ymax=vcmax+vcmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=vcmax),color='red', size=2,data = result_no_soilm)+   ylab(bquote('Vcmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_table$vcmax,na.rm=T)*0,max(Field_measurement_table$vcmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

Field_measurement_based_GPP<-openxlsx::read.xlsx( '2023-04-14_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')
Field_measurement_based_GPP$SITE<-c('ANK','BOB','KOG')

result_no_soilmacaj<-result_no_soilm%>%
  mutate(Aj=gpp/FAPAR/c_molmass* 10^6 / 10000 / 365)%>%
  left_join(Field_measurement_based_GPP%>%dplyr::select(SITE,a_j,FAPAR_field))

Aj_figure<-ggplot(result_no_soilmacaj) +
  geom_bar( aes(x=SITE, y=a_j), stat="identity",data=result_no_soilmacaj, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=Aj),color='red', size=2,data = result_no_soilmacaj)+  ylab(bquote(A[J](mol~C ~ m^-2~day^-1)))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilmacaj$a_j,na.rm=T)*0,max(result_no_soilmacaj$a_j,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

fapar_figure<-ggplot(result_no_soilmacaj) +
  geom_bar( aes(x=SITE, y=FAPAR_field*100), stat="identity",data=result_no_soilmacaj, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=FAPAR*100),color='red', size=2,data = result_no_soilmacaj)+  ylab("fAPAR (%)")+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilmacaj$FAPAR_field,na.rm=T)*0,max(result_no_soilmacaj$FAPAR_field,na.rm=T)*120)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

plot1<- ggarrange(GPP_figure, jmax_figure, vcmax_figure,Aj_figure,fapar_figure, ncol = 5, nrow =1 )
annotate_figure(plot1, top = text_grob("Experiment_Pmodel_P\n(Using field based PPFD, but satellite based fAPAR and modelled LUE)", 
                                      color = "black", face = "bold", size = 10))

#openxlsx::write.xlsx(result_no_soilm,file='H:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_PPFD.xlsx')
result_no_soilm<-openxlsx::read.xlsx('Experiment_Pmodel_PPFD.xlsx')
#ggsave(filename = "F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_PPFD 2024.jpg",width = 6.8, height = 2.7, dpi = 300)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- field fapar + model TRAIT---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



for (i in 1:3){
  temp_result<-Field_measurement_table[i,]
  rp_model_output<-rpmodel(
    tc=temp_result$tc,
    vpd=temp_result$VPDmean,
    co2=temp_result$co2,
    fapar=temp_result$FAPAR_field,
    ppfd=temp_result$ppfd_field ,
    patm = NA,
    elv=temp_result$elv,
    beta = 146,
    soilm = 1,
    meanalpha = 1,
    apar_soilm = 0,
    bpar_soilm = 0.733,
    c4 = FALSE,
    method_jmaxlim = "wang17", #Other options are "smith19" or 'none' "wang17",
    do_ftemp_kphio = TRUE,
    do_soilmstress = FALSE,
    returnvar = NULL,
    verbose = FALSE
  )
  if (i==1) {
    rp_model_output2<-as.data.frame(rp_model_output)
  } else {
    rp_model_output2[i,]<-as.data.frame(rp_model_output)
  }
  rp_model_output2$SITE[i]<-as.character(result[i,'SITE'])
}

result$FAPAR_field<-Field_measurement_table$FAPAR_field

result_no_soilm<-dplyr::left_join(result,rp_model_output2,by="SITE")
result_no_soilm$vcmax<-result_no_soilm$vcmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$vcmax25<-result_no_soilm$vcmax25 * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax<-result_no_soilm$jmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax25<-result_no_soilm$jmax25* 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$gpp<-result_no_soilm$gpp / 10^6 * 10000 * 365 #from g C m-2 day-1 to MgC per ha yr-1
result_no_soilm<-result_no_soilm%>%
  mutate(vcmax=vcmax/FAPAR_field,
         vcmax25 = vcmax25/FAPAR_field,
         jmax = jmax/FAPAR_field,
         jmax25 = jmax25/FAPAR_field,
         iabs_no_fpar=iabs/FAPAR_field)%>%
  dplyr::rename(iabs_with_fpar=iabs)


result_no_soilm$SITE<-c('ANK','BOB','KOG')
Field_measurement_table$SITE<-c('ANK','BOB','KOG')
GPP_figure<-ggplot() +
  geom_bar( aes(x=SITE, y=gpp), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=gpp, ymin=gpp-gpp_se, ymax=gpp+gpp_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=gpp),color='red', size=2,data = result_no_soilm)+
  ylab(bquote('GPP (MgC'~ ha^-1~year^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilm$gpp,na.rm=T)*0,max(Field_measurement_table$gpp,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

jmax_figure<-ggplot(result_no_soilm) +
  geom_bar( aes(x=SITE, y=jmax), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=jmax, ymin=jmax-jmax_se, ymax=jmax+jmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=jmax),color='red', size=2,data = result_no_soilm)+   ylab(bquote('Jmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_table$jmax,na.rm=T)*0,max(Field_measurement_table$jmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

vcmax_figure<-ggplot(result_no_soilm) +
  geom_bar( aes(x=SITE, y=vcmax), stat="identity",data=Field_measurement_table, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_errorbar(aes(x=SITE, y=vcmax, ymin=vcmax-vcmax_se, ymax=vcmax+vcmax_se),alpha=0.3, size=1.3,width=0.4,data=Field_measurement_table)+ 
  geom_point(aes(x=SITE, y=vcmax),color='red', size=2,data = result_no_soilm)+   ylab(bquote('Vcmax_Tair ('*umol ~CO[2]~ m^-2~s^-1*')'))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(Field_measurement_table$vcmax,na.rm=T)*0,max(Field_measurement_table$vcmax,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

Field_measurement_based_GPP<-openxlsx::read.xlsx( '2023-03-15_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')
Field_measurement_based_GPP$SITE<-c('ANK','BOB','KOG')

result_no_soilmacaj<-result_no_soilm%>%
  mutate(Aj=gpp/FAPAR_field/c_molmass* 10^6 / 10000 / 365)%>%
  left_join(Field_measurement_based_GPP%>%dplyr::select(SITE,a_j,FAPAR_field))

Aj_figure<-ggplot(result_no_soilmacaj) +
  geom_bar( aes(x=SITE, y=a_j), stat="identity",data=result_no_soilmacaj, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=Aj),color='red', size=2,data = result_no_soilmacaj)+  ylab(bquote(A[J](mol~C ~ m^-2~day^-1)))+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilmacaj$a_j,na.rm=T)*0,max(result_no_soilmacaj$a_j,na.rm=T)*1.2)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

fapar_figure<-ggplot(result_no_soilmacaj) +
  geom_bar( aes(x=SITE, y=FAPAR_field*100), stat="identity",data=result_no_soilmacaj, fill=result_no_soilm$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=FAPAR_field*100),color='red', size=2,data = result_no_soilmacaj)+  ylab("fAPAR (%)")+
  xlab("Study sites")+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(result_no_soilmacaj$FAPAR_field,na.rm=T)*0,max(result_no_soilmacaj$FAPAR_field,na.rm=T)*120)) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

plot2<- ggarrange(GPP_figure, jmax_figure, vcmax_figure,Aj_figure,fapar_figure, ncol = 5, nrow = 1)+ggtitle('')
annotate_figure(plot2, top = text_grob("Experiment Pmodel_Pf\n (Using field based PPFD and fAPAR, but modelled LUE)", 
                                      color = "black", face = "bold", size = 10))

#ggsave(filename = "F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment P_model_fAPAR_2024.jpg",width = 6.8, height = 2.7, dpi = 300)

#result_no_soilm<-openxlsx::read.xlsx('Experiment P_model_fAPAR.xlsx')
#openxlsx::write.xlsx(result_no_soilm,file='F:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment P_model_fAPAR.xlsx')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ AJ at growth condition  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#L_term=1/sqrt(1+(4*kphio*iabs/jmax)^2)

# L_term * mj could be apple to apple compared to mprime


#AJ_growth = kphio* iabs * mj * L_term

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ check coordination hypothesis  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#AC_growth = AJ_growth



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- satellite fapar + satellite ppfd + model trait--------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



for (i in 1:3){
  temp_result<-Field_measurement_table[i,]
  rp_model_output<-rpmodel(
    tc=temp_result$tc,
    vpd=temp_result$VPDmean,
    co2=temp_result$co2,
    fapar=temp_result$FAPAR,
    ppfd=temp_result$ppfd ,
    patm = NA,
    elv=temp_result$elv,
    beta = 146,
    soilm = 1,
    meanalpha = 1,
    apar_soilm = 0,
    bpar_soilm = 0.733,
    c4 = FALSE,
    method_jmaxlim = "wang17", #Other options are "smith19" or 'none' "wang17",
    do_ftemp_kphio = TRUE,
    do_soilmstress = FALSE,
    returnvar = NULL,
    verbose = FALSE
  )
  if (i==1) {
    rp_model_output2<-as.data.frame(rp_model_output)
  } else {
    rp_model_output2[i,]<-as.data.frame(rp_model_output)
  }
  rp_model_output2$SITE[i]<-as.character(result[i,'SITE'])
}

result$FAPAR<-Field_measurement_table$FAPAR

result_no_soilm<-dplyr::left_join(result,rp_model_output2,by="SITE")
result_no_soilm$vcmax<-result_no_soilm$vcmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$vcmax25<-result_no_soilm$vcmax25 * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax<-result_no_soilm$jmax * 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$jmax25<-result_no_soilm$jmax25* 10^6 /24/3600 #from mol C m-2 day-1 to umol m-2 s-1
result_no_soilm$gpp<-result_no_soilm$gpp / 10^6 * 10000 * 365 #from g C m-2 day-1 to MgC per ha yr-1
result_no_soilm<-result_no_soilm%>%
  mutate(vcmax=vcmax/FAPAR,
         vcmax25 = vcmax25/FAPAR,
         jmax = jmax/FAPAR,
         jmax25 = jmax25/FAPAR,
         iabs_no_fpar=iabs/FAPAR)%>%
  dplyr::rename(iabs_with_fpar=iabs)


openxlsx::write.xlsx(result_no_soilm,file= paste0('H:/Oxford/Chapter_three/github_version/4.Figure_two_GPP_from_measured_Vcmax/Experiment Pmodel_0',Sys.Date(),'.xlsx'))


#================
# codes for documentation

jmax = Jmax_onepoint_tair / (10^6 /24/3600),
vcmax = Vcmax_onepoint_tair / (10^6 /24/3600), # mol C m-2 day-1 
patm = calc_patm(elv),
co2 = 414,
kphio = (0.352+0.021*tc-tc^2 * 3.4/10^4)/8,  
# kphio is from Peng et al New phytologist 2020 
ca = co2_to_ca( co2, patm ),
## photorespiratory compensation point - Gamma-star (Pa)
gammastar = calc_gammastar( tc, patm ),
## Michaelis-Menten coef. (Pa)
kmm = calc_kmm( tc, patm ),   ## XXX Todo: replace 'NA' here with 'patm'
## viscosity correction factor = viscosity( temp, press )/viscosity( 25 degC, 1013.25 Pa)
ns      = viscosity_h2o( tc, patm ),  # Pa s
ns25    = viscosity_h2o( kTo, kPo ),  # Pa s
ns_star = ns / ns25,  # (unitless)
xi  = sqrt( (beta * ( kmm + gammastar ) ) / ( 1.6 * ns_star ) ),
chi = gammastar / ca + ( 1.0 - gammastar / ca ) * xi / ( xi + sqrt(VPDmean) ),
ci = chi * ca,
gamma = gammastar / ca,
kappa = kmm / ca,
## use chi for calculating mj
mj = (chi - gamma) / (chi + 2 * gamma),
## mc
mc = (chi - gamma) / (chi + kappa),
## mj:mv
mjoc = (chi + kappa) / (chi + 2 * gamma), 
a_c = vcmax * (ci - gammastar) / (ci + kmm),
fact_jmaxlim = 1/(sqrt(1+(4*kphio*ppfd_field /jmax)^2)),
a_j = kphio * ppfd_field  * (ci - gammastar)/(ci + 2 * gammastar) * fact_jmaxlim, 
# unit is mol C /m2 /day 
LUE = a_j * c_molmass / (ppfd_field /4.6) , 
# ppfd is in unit (mol /m2 / day), use 4.6 to convert to MJ/m2/day, now LUE is in gC/MJ
a_coordinated=(a_j+a_j)/2, 
# follow A_j only after checking coordination hypothesis
gpp_calculated = a_coordinated *FAPAR_field*  c_molmass / 10^6 * 10000 * 365 
