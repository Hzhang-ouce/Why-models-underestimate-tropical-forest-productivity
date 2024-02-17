
#...............................................................................
#                                                                              .
#  I used this script to draw barplot to compare Pmodel with field measurement .
#                                                                              .
#...............................................................................

setwd('H:/Oxford/Chapter_three/github_version/3.supplementary_compare_climate_variable/')
rm(list=ls())

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

source('H:/Oxford/Chapter_three/github_version/Rpmodel_V3/subroutines.R' )
scores_na <- function(x, ...) {
  not_na <- !is.na(x)
  scores <- rep(NA, length(x))
  scores[not_na] <- outliers::scores(na.omit(x), ...)
  scores
}

#The main reference is this paper, by Donald F. Gatz and Luther Smith, where 3
#formula based estimators are compared with bootstrap results. The best
#approximation to the bootstrap result comes from Cochran (1977):
# https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
# the above method is wrong, now replaced with:
# https://githubmemory.com/repo/harrelfe/Hmisc/issues/138
# https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Statistical_properties

weighted.var.se <- function(x, weights, na.rm = TRUE){
  var <- Hmisc::wtd.var(x, weights, na.rm)
  weights <- sum( (weights / sum(weights))^2 )
  
  sqrt(var*weights)
}

standard_error_calc <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

# significant test was done with Z test
simple_z_test<-function(mean1,mean2,se1,se2){
  z_stat <- (mean1 - mean2) / 
    sqrt(se1^2 + se2^2)
  
  p_value<-1 - pnorm(abs(z_stat)) + pnorm(-abs(z_stat))
  
  return(data.frame(z_stat,p_value))
}




List_of_site<-c('ANK','BOB','KOG')
load('../2_Figure_four_timeseries_of_fapar/organized_climate_withppfd_two_fapar.rda')
P_model_raw<-result%>%
  mutate(SITE = Group.1,
         jmax25_to_jmax=ftemp_inst_jmax( grow_temp , grow_temp , tcref = 25.0 ),
         vcmax25_to_vcmax=ftemp_inst_vcmax( grow_temp , grow_temp , tcref = 25.0 ))

Field_measurement_table<-as.data.frame(List_of_site)%>%
  dplyr::rename(SITE=List_of_site)%>%
  left_join(.,P_model_raw%>%dplyr::select(SITE,jmax25_to_jmax,vcmax25_to_vcmax))

# then run the default P_model
setwd("H:/Oxford/Chapter_three/github_version/Rpmodel_V3")
files.sources = list.files(pattern ='*.R')
sapply(files.sources, source)
setwd('H:/Oxford/Chapter_three/github_version/3.supplementary_compare_climate_variable/')


colnames(result)[1]<-'SITE'
result$fapar_no_cloud<-result$fapar_no_cloud/100
result$ppfd<- result$ppfd/30 # from mol/m2/month to mol/m2/d,
result$VPDmean=result$VPDmean*1000

for (i in 1:3){
  temp_result<-result[i,]
  rp_model_output<-rpmodel(
    tc=temp_result$Tmean,
    vpd=temp_result$VPDmean,
    co2=temp_result$co2,
    fapar=temp_result$fapar_no_cloud,
    ppfd=temp_result$ppfd,
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
  mutate(vcmax=vcmax/fapar_no_cloud,
         vcmax25 = vcmax25/fapar_no_cloud,
         jmax = jmax/fapar_no_cloud,
         jmax25 = jmax25/fapar_no_cloud,
         iabs_no_fpar=iabs/fapar_no_cloud)%>%
  dplyr::rename(iabs_with_fpar=iabs)

##openxlsx::write.xlsx(result_no_soilm,file= paste0('P_model_raw',Sys.Date(),'.xlsx'))

P_model_raw<-openxlsx::read.xlsx('P_model_raw2023-03-14.xlsx')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- PART.A FIELD MEASUREMENTS---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                GPP                                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Trait_big_table2<-read_csv("all_GPP_together_per_SITE_20221122.csv")%>%
  dplyr::select(SITE, GPP,GPP_se)%>%
  mutate(mean_value_trandformed=GPP,
          standard_d_trandformed=GPP_se)



#..........................significant test for GPP...........................



ghana <- Trait_big_table2

library(tidyverse)

List_of_plot<-t(combn(unique(ghana$SITE), 2))
itself<-cbind(unique(ghana$SITE),unique(ghana$SITE))
List_of_plot<-rbind(List_of_plot,itself)
List_of_plot<-as.data.frame(List_of_plot)
colnames(List_of_plot)<-c('plot1','plot2')
List_of_plot$z_statistic=NA
List_of_plot$p_value=NA


for (i in 1:nrow(List_of_plot)) {
  A<- ghana%>%
    filter(SITE == List_of_plot[i,1])
  B<- ghana%>%
    filter(SITE == List_of_plot[i,2])
  anyway<-simple_z_test(A$mean_value_trandformed,B$mean_value_trandformed,A$standard_d_trandformed,B$standard_d_trandformed)
  List_of_plot[i,3]<-anyway$z_stat
  List_of_plot[i,4]<-anyway$p_value
}
output_pairwise<-(xtabs(p_value ~ plot1 + plot2, data=List_of_plot))
# So KOG is different to the ANK and BOB 

Letter_significance<- data.frame(
  Letters = c("a","a","b"), 
  SITE = c("ANK","BOB","KOG"))
Trait_big_table_cwm<-Trait_big_table2%>%
  mutate(mean_original = mean_value_trandformed,
         upper = (mean_value_trandformed+ standard_d_trandformed),
         lower = (mean_value_trandformed- standard_d_trandformed))%>%
  left_join(Letter_significance)%>%
  left_join(P_model_raw, by="SITE")

Field_measurement_table<-Trait_big_table_cwm%>%
  dplyr::select(SITE,mean_original,standard_d_trandformed)%>%
  dplyr::rename(gpp=mean_original,
                gpp_se = standard_d_trandformed)%>%
  left_join(Field_measurement_table,.,by='SITE')

library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggsci)
library(ggpubr)

#List_of_f
cwm_table<-Trait_big_table_cwm
ylabel_name<-"GPP"
List_of_site<-c('KOG','BOB','ANK')
cls <- data.frame(SITE=List_of_site, colour=c("#deebf7","#9ecae1","#3182bd"))
colnames(cwm_table)[c(1,4)]<-c("SITE","value")
#cwm_table$standard_dev<-as.numeric(cwm_table$standard_dev)  #change variance into standard deviation
#cwm_table[,2]<-as.numeric(cwm_table[,2])
cwm_table <- left_join(cwm_table, cls, by="SITE")
cwm_table$SITE <- factor(cwm_table$SITE, levels = c("ANK", "BOB", "KOG"))
# 
# cwm_table$SITE <- str_replace(cwm_table$SITE, "ANK", "ANK (wet)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "BOB", "BOB (mid)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "KOG", "KOG (dry)")


ggplot(cwm_table) +
  geom_bar( aes(x=SITE, y=value), stat="identity", fill=cwm_table$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=GPP),color='red', size=2)+
  geom_errorbar(aes(x=SITE, y=value, ymin=lower, ymax=upper),alpha=0.3, size=1.3,width=0.4)+ theme_bw()+
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(cwm_table$value,na.rm=T)*0.6,max(cwm_table$upper,na.rm=T)*1.2)) +
  geom_text(y = max(cwm_table$upper)*1.18,aes(x=SITE, y=value,label=Letters)) +
  labs(title = ylabel_name) +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 
#ggsave(filename = "GPP_data_model_compar.jpg",width = 1.74, height = 2.4, dpi = 300) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    ci/ca                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



List_of_site<-c('KOG01','KOG03','KOG04','BOB01','BOB02' ,'ANK01','ANK03')


#the left join will bring species to the twig density table
Census_big_table<-read.csv("H:/Oxford/Chapter_one/input_data/Census_data/Census_table_all_plots_Species_list.csv")%>%
  filter(grepl('^[0-9]{1,3}$', Tag.No))%>%
  mutate(Tag_ID=sprintf("%03d", as.numeric(Tag.No)))%>%
  mutate(PLOT_TREE=paste0(plot_code,'_T',Tag_ID))

ANK_basal<-read.csv('H:/Oxford/Chapter_one/input_data/Census_data/ANKSpecies_list.csv')%>%mutate(SITE='ANK')
BOB_basal<-read.csv('H:/Oxford/Chapter_one/input_data/Census_data/BOBSpecies_list.csv')%>%mutate(SITE='BOB')
KOG_basal<-read.csv('H:/Oxford/Chapter_one/input_data/Census_data/KOGSpecies_list.csv')%>%mutate(SITE='KOG')

species_basal<-rbind(ANK_basal,BOB_basal,KOG_basal)


get_chi_from_13C<-function(delta_13C){
  compensation=42
  Ca=404
  lower_f=0.016
  asss=0.0044
  bsss=0.03
  delta_13C_air = -8.7 #see reference here https://zenodo.org/record/569501#.YGGCe68zbLs
  Big_delta_13C = (delta_13C_air - delta_13C)/(1+delta_13C/1000)
  Chi_yunke = (Big_delta_13C/1000 - asss + lower_f * compensation / Ca)/(bsss - asss)
  Chi_wang = (Big_delta_13C/1000 - asss) / (0.027 - asss) 
  return(list(Big_delta_13C,Chi_wang,Chi_yunke))
} #note that delta_13C is in unit 1/1000


Trait_big_table=openxlsx::read.xlsx('H:/Oxford/Chapter_one/input_data/Overall_trait_raw_data.xlsx',sheet='C13')%>%
  dplyr::rename(C13=!!as.name(colnames(.)[2]))%>%
  separate("ID", sep='-',into=c("PLOT","TREE"),remove = F)%>%
  mutate(PLOT_TREE=paste0(PLOT,'_',TREE))%>%
  mutate(Chi_chi=get_chi_from_13C(C13)[[2]])%>%
  left_join(Census_big_table[,c('PLOT_TREE','Species')],by="PLOT_TREE")

Trait_big_table<-drop_na(Trait_big_table,any_of('Species'))%>%
  mutate(Outlier_id=outliers::scores((Chi_chi),type="iqr"))%>%
  filter(Outlier_id<1.5)%>%
  mutate(SITE = substr(PLOT,1,3),
         Species_SITE = paste0(Species , SITE))

# Count number of measurements
nom <- Trait_big_table%>%group_by(SITE)%>%
  summarise(NumberOfMeasurements=sum(!is.na(Chi_chi)))

# Check normal distribution
plot_histogram=qplot(Chi_chi, data=Trait_big_table, geom="histogram") 
plot_histogram

# The data is normally distributed

Trait_big_table2<-Trait_big_table%>%
  left_join(species_basal[,c('Species','total_basal','SITE')],by=c('Species','SITE'))%>%
  rowwise()%>%
  mutate(number_record= (sum(as.vector(Trait_big_table$Species_SITE)==Species_SITE)))%>%
  # This is to count how many individuals are measured under one species, the basal area is the total basal area for a species in one plot, but in Trait_big_table, only some individuals are presented
  mutate(weightsss = total_basal/ number_record)%>%
  mutate(SITE=as.factor(SITE))


# lm for significant test
mod <- lm(Chi_chi ~ SITE, data = Trait_big_table2, weights = weightsss)
summary(glht.mod <- glht(mod, mcp(SITE = "Tukey")))
mod.cld <- cld(glht.mod ,weights = weightsss)
plot(mod.cld)

Letter_significance <- as.data.frame(mod.cld$mcletters$Letters)%>%
  rename(Letters=!!as.name('mod.cld$mcletters$Letters'))%>%
  rownames_to_column(var='SITE')

Trait_big_table_cwm<-Trait_big_table2%>%
  group_by(SITE)%>%
  summarise(mean_value_trandformed = wtd.mean(Chi_chi,weightsss,na.rm=T),
            standard_d_trandformed=weighted.var.se(Chi_chi,weightsss,na.rm=T))%>%
  mutate(mean_original = mean_value_trandformed,
         upper = (mean_value_trandformed+ standard_d_trandformed),
         lower = (mean_value_trandformed- standard_d_trandformed))%>%
  left_join(Letter_significance)%>%
  left_join(nom, by="SITE")%>%
  left_join(P_model_raw, by="SITE")


Field_measurement_table<-Trait_big_table_cwm%>%
  dplyr::select(SITE,mean_original,standard_d_trandformed)%>%
  dplyr::rename(cica=mean_original,
                cica_se = standard_d_trandformed)%>%
  left_join(Field_measurement_table,.,by='SITE')

library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggsci)
library(ggpubr)

#List_of_f

cwm_table<-Trait_big_table_cwm
ylabel_name<-"ci/ca"
List_of_site<-c('KOG','BOB','ANK')
cls <- data.frame(SITE=List_of_site, colour=c("#deebf7","#9ecae1","#3182bd"))
#cwm_table$standard_dev<-as.numeric(cwm_table$standard_dev)  #change variance into standard deviation
#cwm_table[,2]<-as.numeric(cwm_table[,2])
cwm_table <- left_join(cwm_table, cls, by="SITE")
cwm_table$SITE <- factor(cwm_table$SITE, levels = c("ANK", "BOB", "KOG"))

# cwm_table$SITE <- str_replace(cwm_table$SITE, "ANK", "ANK (wet)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "BOB", "BOB (mid)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "KOG", "KOG (dry)")
ggplot(cwm_table) +
  geom_bar( aes(x=SITE, y=mean_original), stat="identity", fill=cwm_table$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=chi),color='red', size=2)+
  geom_errorbar(aes(x=SITE, y=mean_original, ymin=lower, ymax=upper),alpha=0.3, size=1.3,width=0.4)+ theme_bw()+
  ylab("ci/ca (unitless)")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(cwm_table$mean_original,na.rm=T)*0.6,max(cwm_table$upper,na.rm=T)*1.2)) +
  geom_text(y = max(cwm_table$upper)*1.18,aes(x=SITE, y=mean_original,label=Letters)) +

  theme(title = element_text(size=9),
        plot.title = element_markdown()) +
  geom_text(y = max(cwm_table$upper)*1.07,aes(x=SITE, y=mean_original,label=NumberOfMeasurements), colour="#636363", size=3)


#ggsave(filename = "cica_data_model_compare.jpg",width = 1.74, height = 2.4, dpi = 300) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    fapar                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FAPAR<-read.csv('../2_Figure_four_timeseries_of_fapar/LAI_monthly_Caneye_illastik_20211208.csv')%>%
  group_by(month,plot_code)%>%
  summarise(FAPAR_field = mean (final_FAPAR,na.rm=T),
            n=n())%>%
  mutate(SITE= substr(plot_code,1,3))%>%
  group_by(SITE)%>%
  summarise(
            FAPAR_field_se = standard_error_calc(FAPAR_field),
            FAPAR_field = mean (FAPAR_field))

Field_measurement_table<-FAPAR%>%
  dplyr::select(SITE,FAPAR_field)%>%
  left_join(Field_measurement_table,.,by='SITE')


cwm_table<-FAPAR
cwm_table$FAPAR<-result$fapar_no_cloud

  List_of_site<-c('KOG','BOB','ANK')
  cls <- data.frame(SITE=List_of_site, colour=c("#deebf7","#9ecae1","#3182bd"))
  cwm_table <- left_join(cwm_table, cls, by="SITE")
  #cwm_table$standard_dev<-as.numeric(cwm_table$standard_dev)  #change variance into standard deviation
  #cwm_table[,2]<-as.numeric(cwm_table[,2])
  cwm_table$SITE <- factor(cwm_table$SITE, levels = c("ANK", "BOB", "KOG"))
  # cwm_table$SITE <- str_replace(cwm_table$SITE, "ANK", "ANK (wet)")
  # cwm_table$SITE <- str_replace(cwm_table$SITE, "BOB", "BOB (mid)")
  # cwm_table$SITE <- str_replace(cwm_table$SITE, "KOG", "KOG (dry)")
  
  
ggplot(cwm_table) +
    geom_bar( aes(x=SITE, y=FAPAR_field), stat="identity", fill=cwm_table$colour, alpha=0.7,width = 0.8) +
    geom_point(aes(x=SITE, y=FAPAR),color='red', size=2)+
    geom_errorbar(aes(x=SITE, y=FAPAR_field, ymin=FAPAR_field-FAPAR_field_se, ymax=FAPAR_field+FAPAR_field_se),alpha=0.3, size=1.3,width=0.4)+ theme_bw()+
    ylab("FAPAR (unitless)")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian( ylim = c(min(cwm_table$FAPAR_field,na.rm=T)*0.6,max(cwm_table$FAPAR_field,na.rm=T)*1.2)) +

    theme(title = element_text(size=9),
          plot.title = element_markdown()) 
#ggsave(filename = "fapar_data_model_compare.jpg",width = 1.74, height = 2.4, dpi = 300) 
  



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    ppfd                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sum_with_error <- function(x, na.rm=TRUE){
  if (na.rm) x <- na.omit(x)
  x2 <- x^2
  sqrt(sum(x2))
}


J_to_mol = 4.6 # this will convert J m-2 s-1 (= W m-2) to umol (quanta) m-2 s-1


ANK_ppfd<-read.csv('H:/Oxford/Chapter_one/From_imma/Niko_s_daily_climate/ANK_daily_climate.csv') # It is w/m2 in this file for shortwave radiation
BOB_ppfd<-read.csv('H:/Oxford/Chapter_one/From_imma/Niko_s_daily_climate/BOB_daily_climate.csv') # It is w/m2 in this file for shortwave radiation
KOG_ppfd<-read.csv('H:/Oxford/Chapter_one/From_imma/Niko_s_daily_climate/KOG_daily_climate.csv') # It is w/m2 in this file for shortwave radiation

result$ppfd_field[result$SITE=='ANK'] <- mean(ANK_ppfd$Rad)*J_to_mol / 1000000 *3600*24 *0.5 #the 0.5 is photosynthetically active fraction (which is called PAR, see Wang et al Nature plants)
result$ppfd_field[result$SITE=='BOB'] <- mean(BOB_ppfd$Rad)*J_to_mol / 1000000 *3600*24 *0.5
result$ppfd_field[result$SITE=='KOG'] <- mean(KOG_ppfd$Rad)*J_to_mol / 1000000 *3600*24 *0.5

result$ppfd_field_se[result$SITE=='ANK'] <- mean(ANK_ppfd$Rad_sd)*J_to_mol / 1000000 *3600*24 *0.5
result$ppfd_field_se[result$SITE=='BOB'] <- mean(BOB_ppfd$Rad_sd)*J_to_mol / 1000000 *3600*24 *0.5
result$ppfd_field_se[result$SITE=='KOG'] <- mean(KOG_ppfd$Rad_sd)*J_to_mol / 1000000 *3600*24 *0.5

cwm_table<-result

List_of_site<-c('KOG','BOB','ANK')
cls <- data.frame(SITE=List_of_site, colour=c("#deebf7","#9ecae1","#3182bd"))
cwm_table <- left_join(cwm_table, cls, by="SITE")
#cwm_table$standard_dev<-as.numeric(cwm_table$standard_dev)  #change variance into standard deviation
#cwm_table[,2]<-as.numeric(cwm_table[,2])
cwm_table$SITE <- factor(cwm_table$SITE, levels = c("ANK", "BOB", "KOG"))
# cwm_table$SITE <- str_replace(cwm_table$SITE, "ANK", "ANK (wet)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "BOB", "BOB (mid)")
# cwm_table$SITE <- str_replace(cwm_table$SITE, "KOG", "KOG (dry)")


ggplot(cwm_table) +
  geom_bar( aes(x=SITE, y=ppfd_field), stat="identity", fill=cwm_table$colour, alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=ppfd),color='red', size=2)+
  geom_errorbar(aes(x=SITE, y=ppfd_field, ymin=ppfd_field-ppfd_field_se, ymax=ppfd_field+ppfd_field_se),alpha=0.3, size=1.3,width=0.4)+ theme_bw()+
  ylab("PPFD (mol/m2/d)")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian( ylim = c(min(cwm_table$ppfd_field,na.rm=T)*0.6,max(cwm_table$ppfd_field,na.rm=T)*1.2)) +
  labs(title = "") +
  theme(title = element_text(size=9),
        plot.title = element_markdown()) 

#ggsave(filename = "ppfd_data_model_compare.jpg",width = 1.74, height = 2.4, dpi = 300) 

Field_measurement_table<-result%>%
  dplyr::select(SITE,ppfd_field,ppfd_field_se)%>%
  left_join(Field_measurement_table,.,by='SITE')




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  vcmax and jmax with the one point method                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is made by Eleanor, I have made a new one
# One_point_method<-read.csv('H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Ghana_VcmaxJmax.csv')%>%
#   # They are all sun leaves already
#   dplyr::select(Treecode,Vcmax_400,Jmax_1200,Asat)%>%
#   separate(Treecode,c('PLOT','TREE'),sep='-')%>%
#   rename(Vcmax_onepoint=Vcmax_400, Jmax_onepoint=Jmax_1200)%>%
#   mutate(PLOT_TREE=paste0(PLOT,'_',TREE))


field_table<-Field_measurement_table


One_point_method<-openxlsx::read.xlsx('../Vcmax_Jmax_one_piont_made_by_Eleanor/Convert_Asat_Amax_to_Vcmax_Huanyuan20221021.xlsx')%>%
  mutate(Vcmax_onepoint=Vcmax_onepoint_30C/ftemp_inst_vcmax( Mean.Tleafasat , Mean.Tleafasat , tcref = 25.0 ),
         Jmax_onepoint= Jmax_onepoint_30C /ftemp_inst_jmax( Mean.Tleafamax , Mean.Tleafamax , tcref = 25.0)
         #Jmax_onepoint= Jmax_onepoint_30C, Vcmax_onepoint =Vcmax_onepoint_30C 
  )%>%
  # They are all sun leaves already
  dplyr::select(Treecode,Vcmax_onepoint,Jmax_onepoint,Asat)%>%
  separate(Treecode,c('PLOT','TREE'),sep='-')%>%
  mutate(PLOT_TREE=paste0(PLOT,'_',TREE))



#the left join will bring species to the twig density table
Census_big_table<-read.csv("H:/Oxford/Chapter_one/input_data/Census_data/Census_table_all_plots_Species_list.csv")%>%
  filter(grepl('^[0-9]{1,3}$', Tag.No))%>%
  mutate(Tag_ID=sprintf("%03d", as.numeric(Tag.No)))%>%
  mutate(PLOT_TREE=paste0(plot_code,'_T',Tag_ID))
Trait_big_table<-left_join(One_point_method,Census_big_table[c('Species','PLOT_TREE','WD')],by="PLOT_TREE")
Trait_big_table<-drop_na(Trait_big_table,any_of('Species'))%>%
  drop_na(.,any_of('Vcmax_onepoint'))%>%
  mutate(Outlier_id=outliers::scores((Vcmax_onepoint),type="iqr"))%>%
  #filter(Outlier_id<1.5)%>%
  mutate(SITE=substr(PLOT,1,3))%>%
  mutate(Species_SITE = paste0(Species,SITE))

# Count number of measurements
nom <- Trait_big_table%>%group_by(SITE)%>%
  summarise(NumberOfMeasurements=sum(!is.na(Vcmax_onepoint)))

# Check normal distribution
plot_histogram=qplot(sqrt(Vcmax_onepoint), data=Trait_big_table, geom="histogram") 
plot_histogram

##ggsave(plot_histogram,file="Jmax_ACi_25C_hist.jpg",width = 3, height = 3)
# The data is normally distributed


Trait_big_table2<-Trait_big_table%>%
  left_join(species_basal[,c('Species','total_basal','SITE')],by=c('Species','SITE'))%>%
  rowwise()%>%
  mutate(number_record= (sum(as.vector(Trait_big_table$Species_SITE)==Species_SITE)))%>%
  # This is to count how many individuals are measured under one species, the basal area is the total basal area for a species in one plot, but in Trait_big_table, only some individuals are presented
  mutate(weightsss = total_basal/ number_record)%>%
  mutate(SITE=as.factor(SITE))


# lm for significant test
mod <- lm(Vcmax_onepoint ~ SITE, data = Trait_big_table2)
summary(glht.mod <- glht(mod, mcp(SITE = "Tukey")))
mod.cld <- cld(glht.mod)
plot(mod.cld)

Letter_significance <- as.data.frame(mod.cld$mcletters$Letters)%>%
  rename(Letters=!!as.name('mod.cld$mcletters$Letters'))%>%
  rownames_to_column(var='SITE')

Trait_big_table_cwm<-Trait_big_table2%>%
  group_by(SITE)%>%
  summarise(
    Vcmax_onepoint_se=weighted.var.se(Vcmax_onepoint,weightsss,na.rm=T),
    Vcmax_onepoint_25C = wtd.mean(Vcmax_onepoint,weightsss,na.rm=T))%>%
  left_join(Letter_significance)%>%
  left_join(nom, by="SITE")%>%
  left_join(P_model_raw, by="SITE")



field_table_ggplot<-left_join(field_table,Trait_big_table_cwm%>%dplyr::select(SITE,Vcmax_onepoint_25C,Vcmax_onepoint_se))


#the left join will bring species to the twig density table
Census_big_table<-read.csv("H:/Oxford/Chapter_one/input_data/Census_data/Census_table_all_plots_Species_list.csv")%>%
  filter(grepl('^[0-9]{1,3}$', Tag.No))%>%
  mutate(Tag_ID=sprintf("%03d", as.numeric(Tag.No)))%>%
  mutate(PLOT_TREE=paste0(plot_code,'_T',Tag_ID))
Trait_big_table<-left_join(One_point_method,Census_big_table[c('Species','PLOT_TREE','WD')],by="PLOT_TREE")
Trait_big_table<-drop_na(Trait_big_table,any_of('Species'))%>%
  drop_na(.,any_of('Jmax_onepoint'))%>%
  mutate(Outlier_id=outliers::scores((Jmax_onepoint),type="iqr"))%>%
  #filter(Outlier_id<1.5)%>%
  mutate(SITE=substr(PLOT,1,3))%>%
  mutate(Species_SITE = paste0(Species,SITE))

# Count number of measurements
nom <- Trait_big_table%>%group_by(SITE)%>%
  summarise(NumberOfMeasurements=sum(!is.na(Jmax_onepoint)))

# Check normal distribution
plot_histogram=qplot(Jmax_onepoint, data=Trait_big_table, geom="histogram") 
plot_histogram
##ggsave(plot_histogram,file="Jmax_ACi_25C_hist.jpg",width = 3, height = 3)
# The data is normally distributed


Trait_big_table2<-Trait_big_table%>%
  left_join(species_basal[,c('Species','total_basal','SITE')],by=c('Species','SITE'))%>%
  rowwise()%>%
  mutate(number_record= (sum(as.vector(Trait_big_table$Species_SITE)==Species_SITE)))%>%
  # This is to count how many individuals are measured under one species, the basal area is the total basal area for a species in one plot, but in Trait_big_table, only some individuals are presented
  mutate(weightsss = total_basal/ number_record)%>%
  mutate(SITE=as.factor(SITE))


# lm for significant test
mod <- lm(Jmax_onepoint ~ SITE, data = Trait_big_table2)
summary(glht.mod <- glht(mod, mcp(SITE = "Tukey")))
mod.cld <- cld(glht.mod)
plot(mod.cld)

Letter_significance <- as.data.frame(mod.cld$mcletters$Letters)%>%
  rename(Letters=!!as.name('mod.cld$mcletters$Letters'))%>%
  rownames_to_column(var='SITE')

Trait_big_table_cwm<-Trait_big_table2%>%
  group_by(SITE)%>%
  summarise(
    Jmax_onepoint_se=weighted.var.se(Jmax_onepoint,weightsss,na.rm=T),
    Jmax_onepoint_25C = wtd.mean(Jmax_onepoint,weightsss,na.rm=T))%>%
  left_join(Letter_significance)%>%
  left_join(nom, by="SITE")%>%
  left_join(P_model_raw, by="SITE")


field_table_ggplot<-left_join(field_table_ggplot,Trait_big_table_cwm%>%dplyr::select(SITE,Jmax_onepoint_25C,Jmax_onepoint_se))%>%
  dplyr::select(SITE,Vcmax_onepoint_25C,Jmax_onepoint_25C,Jmax_onepoint_se,Vcmax_onepoint_se)%>%
  mutate(ratio=Jmax_onepoint_25C/Vcmax_onepoint_25C)


Field_measurement_table<-field_table_ggplot%>%
  dplyr::select(SITE,Vcmax_onepoint_25C,Jmax_onepoint_25C,Jmax_onepoint_se,Vcmax_onepoint_se)%>%
  left_join(field_table,.,by='SITE')

#openxlsx::write.xlsx(Field_measurement_table,file='Field_measurement_photosynthesis_trait.xlsx')

Field_measurement_table<-openxlsx::read.xlsx('Field_measurement_photosynthesis_trait.xlsx')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           relative humudity                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VPD_table<-result%>%
  mutate(
    RH=1-VPDmean*1000 /( 610.7*10^(7.5*Tmean/(237.3+Tmean)))
  )

VPD_table$RH_field_moore[1]<-0.91 # These values are from Moore 2018 global change biology
VPD_table$RH_field_moore[2]<-0.839
VPD_table$RH_field_moore[3]<-0.792

VPD_table<-VPD_table%>%
  mutate(
  VPDmean_from_RH=(1-RH_field_moore)*( 610.7*10^(7.5*Tmean/(237.3+Tmean)))/1000
)


ggplot(VPD_table) +
  geom_bar( aes(x=SITE, y=RH_field_moore*100, fill=SITE ), stat="identity", alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=RH*100),color='red', size=2)+
  ylab("Relative humidity (%)")+
  xlab("")+
  theme(title = element_text(size=9),
        plot.title = element_markdown())+ theme_bw()+
  scale_fill_manual(
    values  = c("#3182bd","#9ecae1","#deebf7"),
    labels = c("Wet", "Mid", "Dry"),
    name = 'Site'
  )+theme(legend.position = 'none')  +theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave(filename = "RH_data_model_compare.jpg",width = 1.74, height = 2.4, dpi = 300)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           temperature                                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ttable<-result


Ttable$T_field_moore[1]<-25 # These values are from Moore 2018 global change biology
Ttable$T_field_moore[2]<-25.7
Ttable$T_field_moore[3]<-26.4


ggplot(Ttable) +
  geom_bar( aes(x=SITE, y=T_field_moore, fill=SITE ), stat="identity", alpha=0.7,width = 0.8) +
  geom_point(aes(x=SITE, y=Tmean),color='red', size=2)+
  ylab("Mean annual\n air temperature (°C)")+
  xlab("")+
  theme(title = element_text(size=9),
        plot.title = element_markdown())+ theme_bw()+
  scale_fill_manual(
    values  = c("#3182bd","#9ecae1","#deebf7"),
    labels = c("Wet", "Mid", "Dry"),
    name = 'Site'
  )+theme(legend.position = 'none')  +theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave(filename = "temperature_data_model_compare.jpg",width = 1.74, height = 2.4, dpi = 300)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           archived codes for CRU                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# temperature
library(raster)
library(ncdf4)
resultXXX<-data.frame(matrix(nrow=8,ncol = 0))
resultXXX$Latitude=c(6.6910,5.2680,7.2616,-3.95,-12.8309,-16.02,-1.7160,-13.0765)
resultXXX$Longitude=c(-1.3389,-2.6955,-1.1501,-73.4333,-69.2705,-62.73,-51.4570,-52.3858) #From Moore 2017
resultXXX$name=c('BOB','ANK','KOG','ALP','TAM','KEN','CAX','Tanguro')
site2<-resultXXX[,c("Longitude","Latitude","name")]
centroid_spdf <- SpatialPointsDataFrame(
  site2[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), site2)
# extract function, this is a simple extract without buffer
extract_fapar<-function(Ghana_map,centroid_spdf){
  #Ghana_map<-raster(tif_address)
  cent_max <- raster::extract(Ghana_map,             # raster layer
                              centroid_spdf,   # SPDF with centroids for buffer
                              method='simple',
                              df=TRUE)         # return a dataframe?
  site2$temp<-rowMeans(cent_max[,2:length(cent_max)])
  return(site2)
}
nc_data <- nc_open('C:/Users/Huanyuan/Downloads/cru_ts4.06.2011.2020.tmp.dat.nc/cru_ts4.06.2011.2020.tmp.dat.nc')
print(nc_data)
#pft <- ncvar_get(nc_data, varid="pft")
nc_close(nc_data)
my_raster<-raster::brick('C:/Users/Huanyuan/Downloads/cru_ts4.06.2011.2020.tmp.dat.nc/cru_ts4.06.2011.2020.tmp.dat.nc',varname= 'tmp')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
dim(my_raster)
# check length is 3840 , ok it is 1440
# we only want the last 10 years
plot(my_raster[[1]])
points(centroid_spdf)
temp_list<-list()
my_raster<-(my_raster[[1:60]])
nice_output_temperature<-extract_fapar(my_raster,centroid_spdf)
nice_output_temperature$temp_moore[1]<-25.7
nice_output_temperature$temp_moore[2]<-25.0
nice_output_temperature$temp_moore[3]<-26.4
# nice_output_temperature$name <- str_replace(nice_output_temperature$name, "ANK", "ANK (wet)")
# nice_output_temperature$name <- str_replace(nice_output_temperature$name, "BOB", "BOB (mid)")
# nice_output_temperature$name <- str_replace(nice_output_temperature$name, "KOG", "KOG (dry)")
ggplot(nice_output_temperature[1:3,]) +
  geom_bar( aes(x=name, y=temp_moore, fill=name ), stat="identity", alpha=0.7,width = 0.8) +
  geom_point(aes(x=name, y=temp),color='red', size=2)+
  ylab("Mean annual temperature (°C)")+
  xlab("")+
  theme(title = element_text(size=9),
        plot.title = element_markdown())+ theme_bw()+
  scale_fill_manual(
    values  = c("#3182bd","#9ecae1","#deebf7"),
    labels = c("Wet", "Mid", "Dry"),
    name = 'Site'
  )+ theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave(filename = "temperature_data_model_compare.jpg",width = 2.58, height = 3.4, dpi = 300)
vap<-raster::brick('cru_ts4.01.1901.2016.vap.dat.nc',varname= 'vap')
Tmin<-raster::brick('cru_ts4.01.1901.2016.tmn.dat.nc',varname= 'tmn')
Tmax<-raster::brick('cru_ts4.01.1901.2016.tmx.dat.nc',varname= 'tmx')
vap<-(vap[[(1392-59):1392]])
Tmin<-(Tmin[[(1392-59):1392]])
Tmax<-(Tmax[[(1392-59):1392]])
Tmax_min <- (8.635 * (Tmax + Tmin))/(0.5 * (Tmax + Tmin) + 237.3)
es0 = 0.611
vpd_hpa <- (es0 * exp(Tmax_min) - (0.10 * vap)) * 10. # kPa --> hPa
plot(vpd_hpa[[1]])
points(centroid_spdf)
nice_output_vpd<-extract_fapar(vpd_hpa,centroid_spdf)
es1 <- es0 * exp((17.27 * Tmax)/(Tmax+273.3))
vpd1_hpa <- (es1 - 0.1 * vap) * 10 # kPa --> hPa
es2 <- es0 * exp((17.27 * Tmin)/(Tmin+273.3))
vpd2_hpa <- (es2 - 0.1 * vap) * 10 # kPa --> hPa
vpd_hpa <- (vpd1_hpa + vpd2_hpa)/2 #calculating vpd by average the two
cru_vpd<-extract_fapar(vpd_hpa,centroid_spdf)
cru_vpd<-dplyr::rename(cru_vpd,cru_vpd=temp)
cru_SVP<-extract_fapar((es1+es2)/2,centroid_spdf)
cru_vpd$cru_vpd = cru_vpd$cru_vpd/10
cru_vpd$SVP = cru_SVP$temp
cru_vpd<-cru_vpd%>%
  mutate(RH=1-cru_vpd/SVP)
cru_vpd$RH_field_moore[1]<-0.839
cru_vpd$RH_field_moore[2]<-0.91
cru_vpd$RH_field_moore[3]<-0.792
cru_vpd$name <- str_replace(cru_vpd$name, "ANK", "ANK (wet)")
cru_vpd$name <- str_replace(cru_vpd$name, "BOB", "BOB (mid)")
cru_vpd$name <- str_replace(cru_vpd$name, "KOG", "KOG (dry)")
ggplot(cru_vpd[1:3,]) +
  geom_bar( aes(x=name, y=RH_field_moore*100, fill=name ), stat="identity", alpha=0.7,width = 0.8) +
  geom_point(aes(x=name, y=RH*100),color='red', size=2)+
  ylab("Relative humidity (%)")+
  xlab("")+
  theme(title = element_text(size=9),
        plot.title = element_markdown())+ theme_bw()+
  scale_fill_manual(
    values  = c("#3182bd","#9ecae1","#deebf7"),
    labels = c("Wet", "Mid", "Dry"),
    name = 'Site'
  )+theme(legend.position = 'none')  +theme(axis.text.x = element_text(angle = 45, hjust = 1))
