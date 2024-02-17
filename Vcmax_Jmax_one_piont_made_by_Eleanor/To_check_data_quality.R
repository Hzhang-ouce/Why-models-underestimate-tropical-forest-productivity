

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


One_point_method<-read.csv('H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Ghana_VcmaxJmax.csv')%>%
  # They are all sun leaves already
  dplyr::select(Treecode,Vcmax_400,Jmax_1200,Asat)%>%
  separate(Treecode,c('PLOT','TREE'),sep='-')%>%
  rename(Vcmax_onepoint=Vcmax_400, Jmax_onepoint=Jmax_1200)%>%
  mutate(PLOT_TREE=paste0(PLOT,'_',TREE))
