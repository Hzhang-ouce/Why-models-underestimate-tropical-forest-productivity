# This codes are for conenivence in investigating one point method, this is not used for the project eventually. 
rm(list=ls())

# This is eleanor's dataset, it is dry season only, we shouldn't work on this one now, because we have all season, using all season will make vcmax and jmax smaller
ghana_true_A = read_csv("H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Ghana_VcmaxJmax.csv")%>%
  dplyr::select(FieldCode,Treecode,Plot,Asat,Amax,Mean.Tleafamax,Mean.Tleafasat,Mean.Ciasat,Mean.Ciamax,DResp)%>%
  drop_na()%>% mutate(Row_id = row_number(),DResp=-DResp)

#  I modified the equation in plantecophys to use p-model 's Jmax equation: (but the two Jmax equation does not differ much)
files.sources = list.files(pattern ='*.R',path="H:\\Oxford\\Chapter_three\\Vcmax_Jmax_one_piont_made_by_Eleanor\\plantecophys1.4",full.names = T)
sapply(files.sources, source)
# library(plantecophys)

ghana_true_A = openxlsx::read.xlsx("H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Asat_Amax_page_in_Ghana_trait_2015_2016_Kwaeemma_Theresa_campaigns_compiled_by_Huanyuan.xlsx")%>%
  mutate(FieldCode = newtagname,
         Plot=PLOT)%>%
  filter(Branch=="B01S-")%>% # Sun leaves only
  dplyr::select(FieldCode,Treecode,Plot,Asat,Amax,Mean.Tleafamax,Mean.Tleafasat,Mean.Ciasat,Mean.Ciamax,DResp)%>%
  drop_na()%>%
  distinct()%>% mutate(Row_id = row_number(),DResp=-DResp)%>%
  mutate(Mean.Tleafamax=ifelse(Mean.Tleafamax>32,32,Mean.Tleafamax),
         Mean.Tleafasat=ifelse(Mean.Tleafasat>32,32,Mean.Tleafasat))

data2 <- ghana_true_A 

for (i in 1:nrow(data2)) {
  if (i == 1) {
    ghana_data2 <- fitaci(data2[i, ],
                          varnames = list(ALEAF = "Amax", Tleaf = "Mean.Tleafamax", 
                                          Ci = "Mean.Ciamax", PPFD = "PARi", Rd = "DResp"),
                          fitTPU = FALSE, Tcorrect = F, Patm = 100, PPFD = 2000,
                          fitmethod = "onepoint")
  } else {
    tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = "Amax", Tleaf = "Mean.Tleafamax", 
                                  Ci = "Mean.Ciamax", Rd = "DResp", PPFD = "PARi"),
                  fitTPU = FALSE, Tcorrect = F, Patm = 100, PPFD = 2000,
                  fitmethod = "onepoint")
    ghana_data2 <- rbind( ghana_data2 , tmp)
  }
}

ghana_data2 <- ghana_data2 %>% 
  rename(Jmax_onepoint_30C = Jmax)%>%
  dplyr::select(Row_id,Jmax_onepoint_30C)

ghana_true_A<-left_join(ghana_true_A,ghana_data2,by='Row_id')


for (i in 1:nrow(data2)) {
  if (i == 1) {
    ghana_data2 <- fitaci(data2[i, ],
                          varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleafasat", 
                                          Ci = "Mean.Ciasat", PPFD = "PARi", Rd = "DResp"),
                          fitTPU = FALSE, Tcorrect = F, Patm = 100,  PPFD = 2000,
                          fitmethod = "onepoint")
  } else {
    tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleafasat", 
                                  Ci = "Mean.Ciasat", PPFD = "PARi",Rd = "DResp"),
                  fitTPU = FALSE, Tcorrect =F, Patm = 100,  PPFD = 2000,
                  fitmethod = "onepoint")
    ghana_data2 <- rbind( ghana_data2 , tmp)
  }
}

ghana_data2 <- ghana_data2 %>% 
  rename(Vcmax_onepoint_30C = Vcmax)%>%
  dplyr::select(Row_id,Vcmax_onepoint_30C)

ghana_true_A<-left_join(ghana_true_A,ghana_data2,by='Row_id')


# ============================================================================================
# ============================================================================================
# ============================================================================================
# ============================================================================================

field_table<-openxlsx::read.xlsx('H:/Oxford/Chapter_three/Figure_one_compare_Pmodel_with_measurement/Field_measurement_photosynthesis_trait.xlsx')
field_table<-field_table%>%
  dplyr::select(-Vcmax_onepoint_25C,-Jmax_onepoint_25C)
setwd("H:/Oxford/Chapter_one/Ghana_rainfall_trait_variation_optimality_github/input_data")



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
setwd("H:/Oxford/Chapter_one/Ghana_rainfall_trait_variation_optimality_github/input_data")
source('H:/Oxford/Chapter_three/Rpmodel_V3/subroutines.R' )
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


ANK_basal<-read.csv('Census_data/ANKSpecies_list.csv')%>%mutate(SITE='ANK')
BOB_basal<-read.csv('Census_data/BOBSpecies_list.csv')%>%mutate(SITE='BOB')
KOG_basal<-read.csv('Census_data/KOGSpecies_list.csv')%>%mutate(SITE='KOG')

species_basal<-rbind(ANK_basal,BOB_basal,KOG_basal)



List_of_site<-c('ANK','BOB','KOG')

P_model_raw<-read_csv('H:/Oxford/Chapter_three/Rpmodel_V3/p_model_output_without_moisture_20220924.csv')%>%
  mutate(SITE = Group.1,
         jmax25_to_jmax=ftemp_inst_jmax( grow_temp , grow_temp , tcref = 25.0 ),
         vcmax25_to_vcmax=ftemp_inst_vcmax( grow_temp , grow_temp , tcref = 25.0 ))

Field_measurement_table<-as.data.frame(List_of_site)%>%
  dplyr::rename(SITE=List_of_site)%>%
  left_join(.,P_model_raw%>%dplyr::select(SITE,jmax25_to_jmax,vcmax25_to_vcmax))


One_point_method<-ghana_true_A%>%
  mutate(Vcmax_onepoint=Vcmax_onepoint_30C/ftemp_inst_vcmax( Mean.Tleafasat , Mean.Tleafasat , tcref = 25.0 ),
             Jmax_onepoint= Jmax_onepoint_30C /ftemp_inst_jmax( Mean.Tleafamax , Mean.Tleafamax , tcref = 25.0)
         #Jmax_onepoint= Jmax_onepoint_30C, Vcmax_onepoint =Vcmax_onepoint_30C 
         )%>%
  # They are all sun leaves already
  dplyr::select(Treecode,Vcmax_onepoint,Jmax_onepoint,Asat)%>%
  separate(Treecode,c('PLOT','TREE'),sep='-')%>%
  mutate(PLOT_TREE=paste0(PLOT,'_',TREE))



#the left join will bring species to the twig density table
Census_big_table<-read.csv("Census_data/Census_table_all_plots_Species_list.csv")%>%
  filter(grepl('^[0-9]{1,3}$', Tag.No))%>%
  mutate(Tag_ID=sprintf("%03d", as.numeric(Tag.No)))%>%
  mutate(PLOT_TREE=paste0(plot_code,'_T',Tag_ID))
Trait_big_table<-left_join(One_point_method,Census_big_table[c('Species','PLOT_TREE','WD')],by="PLOT_TREE")
Trait_big_table<-drop_na(Trait_big_table,any_of('Species'))%>%
  drop_na(.,any_of('Vcmax_onepoint'))%>%
  mutate(Outlier_id=outliers::scores((Vcmax_onepoint),type="iqr"))%>%
  filter(Outlier_id<1.5)%>%
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



field_table_ggplot<-left_join(field_table,Trait_big_table_cwm%>%dplyr::select(SITE,Vcmax_onepoint_25C))


#the left join will bring species to the twig density table
Census_big_table<-read.csv("Census_data/Census_table_all_plots_Species_list.csv")%>%
  filter(grepl('^[0-9]{1,3}$', Tag.No))%>%
  mutate(Tag_ID=sprintf("%03d", as.numeric(Tag.No)))%>%
  mutate(PLOT_TREE=paste0(plot_code,'_T',Tag_ID))
Trait_big_table<-left_join(One_point_method,Census_big_table[c('Species','PLOT_TREE','WD')],by="PLOT_TREE")
Trait_big_table<-drop_na(Trait_big_table,any_of('Species'))%>%
  drop_na(.,any_of('Jmax_onepoint'))%>%
  mutate(Outlier_id=outliers::scores((Jmax_onepoint),type="iqr"))%>%
  filter(Outlier_id<1.5)%>%
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


field_table_ggplot<-left_join(field_table_ggplot,Trait_big_table_cwm%>%dplyr::select(SITE,Jmax_onepoint_25C))%>%
  dplyr::select(SITE,vcmax,vcmax25,Vcmax_onepoint_25C,jmax,jmax25,Jmax_onepoint_25C)%>%
  mutate(ratio=Jmax_onepoint_25C/Vcmax_onepoint_25C)

print(field_table_ggplot)
