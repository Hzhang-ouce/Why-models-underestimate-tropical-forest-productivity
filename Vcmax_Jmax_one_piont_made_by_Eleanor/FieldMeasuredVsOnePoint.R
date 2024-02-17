## Load packages:
# ***************
library("readxl")
library(plantecophys)
library(dplyr)
library(ggplot2)

## Load data:
# ***********

rm(list=ls())


##Ghana
##data for comparing measured vcmax/jmax to generated vcmax/jmax by one-point method. 
ghanaKwamee = read_excel("H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Vcmax_Kwaeemma.xlsx")

###transform asat and amax 

# Calculation of Vcmax at 400 ppm CO2 (i.e. from Asat):
# -----------------------------------------------------

data <- ghanaKwamee
data2 <- data 


for (i in 1:nrow(data2)) {
  if (i == 1) {
    ghana_data <- fitaci(data2[i, ],
                       varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleaf", 
                                       Ci = "Ciatasat", PPFD = "Min.PAR", Rd = "Amin"),
                       fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
                      fitmethod = "onepoint", na.rm=TRUE)
  } else {
    tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleaf", 
                                  Ci = "Ciatasat", PPFD = "Min.PAR", Rd = "Amin"),
                 fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
                  fitmethod = "onepoint", na.rm=TRUE)
    ghana_data <- rbind( ghana_data , tmp)
  }
}

ghana_data <- ghana_data %>% 
  rename(Vcmax_400 = Vcmax, Jmax_400 = Jmax)


# Calculation of Jmax at?1200 ppm CO2 (i.e. from Amax):
# -----------------------------------------------------

data <- ghanaKwamee
data2 <- data 

for (i in 1:nrow(data2)) {
  if (i == 1) {
    ghana_data2 <- fitaci(data2[i, ],
                        varnames = list(ALEAF = "Amax", Tleaf = "Mean.Tleaf", 
                                        Ci = "Ci.max", PPFD = 2000, Rd = "Amin"),
                        fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
                        fitmethod = "onepoint", na.rm=TRUE)
  } else {
   tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = "Amax", Tleaf = "Mean.Tleaf", 
                                  Ci = "Ci.max", PPFD = 2000, Rd = "Amin"),
                  fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
                fitmethod = "onepoint", na.rm=TRUE)
    ghana_data2 <- rbind( ghana_data2 , tmp)
  }
}

ghana_data2 <- ghana_data2 %>% 
  rename(Vcmax_1200 = Vcmax, Jmax_1200 = Jmax)


##plot graph
ghana_data<-ghana_data%>%
  tidyr::drop_na(Vcmax_ACi_25C,Vcmax_400)
ghanavcmax= ggplot(ghana_data, aes(x=Vcmax_ACi_25C,y=  Vcmax_400)) +  geom_point(shape = 21) + # , aes(fill = species)
  # scale_fill_viridis_d("Species") +
  # guides(fill = F) +
  geom_smooth(method = "lm") +
  xlim(0, 90)+
  ylim(0, 90)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = expression('Ghana V'["cmax"]*'')) +
  theme_bw()+
  theme(plot.subtitle = element_text(size=16))
summary(lm(Vcmax_400~Vcmax_ACi_25C,data=ghana_data))

plot(ghana_data$Vcmax_ACi_25C,ghana_data$Vcmax_400)
# from the above, we know Vcmax_ACi_25C = Vcmax_400*0.81805 + 0.71979

##plot graph 
ghanajmax= ggplot(ghana_data2, aes(x=Jmax_ACi_25C,y=Jmax_1200)) +
  geom_point(shape = 21) + # , aes(fill = species)
  # scale_fill_viridis_d("Species?) +
  # guides(fill = F) +
  geom_smooth(method = "lm") +
  xlim(0, 170)+
  ylim(0,170)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = expression('Ghana J'["max"]*'')) +
  theme_bw()+
  theme(plot.subtitle = element_text(siz=16))

summary(lm(Jmax_ACi_25C~Jmax_1200,data=ghana_data2))
  # from the above, we know Jmax_ACi_25C = Jmax_1200*0.83691  -1.98387
ghana_vcmax_rsq=rsq(ghana_data$Vcmax_ACi_25C, ghana_data$Vcmax_400)
ghana_jmax_rsq=rsq(ghana_data2$Jmax_ACi_25C, ghana_data2$Jmax_1200)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        redo Asat Amax true dataset                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




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
         Mean.Tleafasat=ifelse(Mean.Tleafasat>32,32,Mean.Tleafasat)) # there are some extreme high values

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

openxlsx::write.xlsx(ghana_true_A,file='H:/Oxford/Chapter_three/Vcmax_Jmax_one_piont_made_by_Eleanor/Convert_Asat_Amax_to_Vcmax_Huanyuan20221021.xlsx')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Eleanor's old codes                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




setwd("//linux-filestore.ouce.ox.ac.uk/ecosystems_drones")

###Australia
aus= read_excel("/PanTropical/?hotosynthesis/Processed/Australia_Trait_per_stem_and_Censuses.xlsx", sheet=3)

aus$Vcmax_400=as.numeric(aus$Vcmax_400)
aus$Jmax_1200=as.numeric(aus$Jmax_1200)

aus_vcmax=aus[complete.cases(aus$Vcmax_curve, aus$Vcmax_400),]
aus_jmax=aus[complete.cases(aus$J?ax_curve, aus$Jmax_1200),]

rsq <- function (x, y) cor(x, y) ^ 2
aus_vcmax_rsq=rsq(aus_vcmax$Vcmax_curve, aus_vcmax$Vcmax_400)
aus_jmax_rsq=rsq(aus_jmax$Jmax_curve, aus_jmax$Jmax_1200)

##plot graph 
ausvcmax= ggplot(aus_vcmax, aes(x=Vcmax_curve,y=Vcmax_40?)) +
  geom_point(shape = 21) + # , aes(fill = species)
  # scale_fill_viridis_d("Species") +
  # guides(fill = F) +
  geom_smooth(method = "lm") +
  xlim(0, 90)+
  ylim(0, 90)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = ?xpression('Australia V'["cmax"]*' (??mol/m'^"2"*'s)')) +
  theme_bw()+
  theme(plot.subtitle = element_text(size=16))

##plot graph 
ausjmax= ggplot(aus_jmax, aes(x=Jmax_curve,y=Jmax_1200)) +
  geom_point(shape = 21) + # , aes(fill = species)
  # scale_fill_viridis_d("Species") +
  # guides(f?ll = F) +
  geom_smooth(method = "lm") +
  xlim(0, 170)+
  ylim(0, 170)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = expression('Australia J'["max"]*' (??mol/m'^"2"*'s)')) +
  theme_bw()+
  theme(plot.subtitle = element_text(size=16))


#####
##brazil 

brazil = read.csv("/PanTropical/Photosynthesis/Processed/AciBACABA_calculated.csv")
brazil.raw=?ead.csv("/PanTropical/Photosynthesis/Raw/photosyn_BACABA.csv")

brazil.raw$extra=paste0(brazil.raw$plot_code, '-',brazil.raw$tree_code)
brazil.raw$extra=gsub('TT', 'T', brazil.raw$extra)
brazil_resp=brazil.raw[brazil.raw$type=='dresp',]
brazil$Treegemcode=?sub('TP', 'T', brazil$Treegemcode)
brazil$Treegemcode=gsub('TT', 'T', brazil$Treegemcode)
brazil$Treegemcode=gsub('4R', '4', brazil$Treegemcode)

brazil$Dresp <- brazil_resp$photosynthesis[match(unlist(brazil$Treegemcode), brazil_resp$extra)]


###mean Ci_?sat = 285
#mean ci_amax=1556

data <- brazil[complete.cases(brazil$Asat & brazil$Dresp), ]
data2 <- data %>%
  mutate(PARi = 1800,
         Ci_asat = 285,
         RD=Dresp)

for (i in 1:nrow(data2)) {
  if (i == 1) {
    brazil_data <- fitaci(data2[i, ],
?                      varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleaf", 
                                       Ci = "Ci_asat", PPFD = "PARi", Rd = "RD"),
                       fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
                       fitme?hod = "onepoint")
  } else {
    tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = "Asat", Tleaf = "Mean.Tleaf", 
                                  Ci = "Ci_asat", PPFD = "PARi", Rd = "RD"),
                  fitTPU = FALSE, Tcorrect = TR?E, Patm = 100, 
                  fitmethod = "onepoint")
    brazil_data <- rbind( brazil_data , tmp)
  }
}

brazil_data <- brazil_data %>% 
  rename(Vcmax_400 = Vcmax, Jmax_400 = Jmax)



# Calculation of Jmax at 1200 ppm CO2 (i.e. from Amax):
# --------?--------------------------------------------

data <- brazil[complete.cases(brazil$Amax & brazil$Dresp), ]
data2 <- data %>%
  mutate(PARi = 1800,
         Ci_amax = 1556,
         RD=Dresp)

for (i in 1:nrow(data2)) {
  if (i == 1) {
    brazil_data2 <- f?taci(data2[i, ],
                        varnames = list(ALEAF = 'Amax', Tleaf = "Mean.Tleaf", 
                                        Ci = "Ci_amax", PPFD = "PARi", Rd = "RD"),
                        fitTPU = FALSE, Tcorrect = TRUE, Patm = 100, 
       ?                fitmethod = "onepoint")
  } else {
    tmp <- fitaci(data2[i, ],
                  varnames = list(ALEAF = 'Amax', Tleaf = "Mean.Tleaf", 
                                   Ci = "Ci_amax", PPFD = "PARi", Rd = "RD"),
                  fitTPU?= FALSE, Tcorrect = TRUE, Patm = 100, 
                  fitmethod = "onepoint")
    brazil_data2 <- rbind( brazil_data2 , tmp)
  }
}

brazil_data2 <- brazil_data2 %>% 
  rename(Vcmax_1200 = Vcmax, Jmax_1200 = Jmax)


###remove outlier 
brazil_data$Vcmax_A?i_25C[brazil_data$Vcmax_ACi_25C>75] <- NA
brazil_data=brazil_data[complete.cases(brazil_data$Vcmax_ACi_25C),]

##plot graph 
brazilvcmax= ggplot(brazil_data, aes(x=Vcmax_ACi_25C ,y=Vcmax_400)) +
  geom_point(shape = 21) + # , aes(fill = species)
  # scale_?ill_viridis_d("Species") +
  # guides(fill = F) +
  geom_smooth(method = "lm") +
  xlim(0, 90)+
  ylim(0, 90)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = expression('Brazil V'["cmax"]*'')) +
  theme_bw()+
  theme(plot.sub?itle = element_text(size=16))

##plot graph 
braziljmax= ggplot(brazil_data2, aes(x=Jmax_ACi_25C,y=Jmax_1200)) +
  geom_point(shape = 21) + # , aes(fill = species)
  # scale_fill_viridis_d("Species") +
  # guides(fill = F) +
  geom_smooth(method = "lm") +
? xlim(0, 170)+
  ylim(0, 170)+
  labs(x = "Field measured",
       y = "One-point method",
       subtitle = expression('Brazil J'["max"]*''))+
  theme_bw()+
  theme(plot.subtitle = element_text(size=16))

brazil_vcmax_rsq=rsq(brazil_data$Vcmax_ACi_25C, br?zil_data$Vcmax_400)
brazil_jmax_rsq=rsq(brazil_data2$Jmax_ACi_25C, brazil_data2$Jmax_1200)

  
grid.arrange(ausvcmax, brazilvcmax, ghanavcmax, ausjmax, braziljmax, ghanajmax, ncol=3)


