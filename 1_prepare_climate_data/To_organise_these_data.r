
###########################################################################
###########################################################################
###                                                                     ###
###                            Huanyuan Zhang                           ###
###                      Thu Feb 27 17:36:16 2020                       ###
###   This document is for organizing the climate data shared by Imma   ###
###                                                                     ###
###########################################################################
###########################################################################
setwd('H:/Oxford/Chapter_three/github_version/1_prepare_climate_data')
library(tidyverse)
library(rpmodel)
##------------------------------------------------
##  lets try climate data prepared by Imma first  
##------------------------------------------------
pizzass<-read.csv("ANK_gapfill_erai_monthly_1979_2017.csv")
new_pizza1<-pizzass[which(pizzass$year<2017 & pizzass$year>2010),]
pizzass<-read.csv("BOB_gapfill_erai_monthly_1979_2017.csv")
new_pizza2<-pizzass[which(pizzass$year<2017 & pizzass$year>2010),]
new_pizza2$SWmean<-NaN
pizzass<-read.csv("KOG_gapfill_erai_monthly_1979_2017.csv")
new_pizza3<-pizzass[which(pizzass$year<2017 & pizzass$year>2010),]
pizza<-rbind(new_pizza2,new_pizza1,new_pizza3)
result<-aggregate(pizza[, 3:6], list(pizza$site), mean)
result$grow_temp<-result$Tmax*(1/2+1/pi)+result$Tmin*(1/2-1/pi)

##-----------
##  lat lon CO2  
##-----------
result$Latitude=c(6.6910,5.2680,7.2616)
result$Longitude=c(-1.3389,-2.6955,-1.1501) #From Moore 2017
result$co2=414
result$elv=c(114,235,229)

##--------
##  ppfd  
##--------
library(ncdf4)
library(elevatr)
library(raster)
# firstly, we load wedfi watch data, a big nc file, average them (2006-2015) into one map

#load("organized_climate.rda")
ncin <- nc_open("H:/Oxford/Chapter_one/From_imma/climate_data/1982_2016.ppfd.nc") # The unit is mol/m2/month
ppfd_map <- ncvar_get(ncin,"ppfd")
ppfd_map<-apply(ppfd_map[,,277:420], c(1,2), mean,na.rm=T)##----------------------
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix
ppfd_map<-raster(rotate_it(ppfd_map))
ppfd_map<-flip(ppfd_map,1)
extent(ppfd_map) <- extent(-180, 180, -90, 90) #adjust its extent
crs(ppfd_map) <-'+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
e <- extent(-4, 0, 4, 9)
Ghana_map <- crop(ppfd_map, e)	
plot(Ghana_map)
Ghana_pointlist<-data.frame(rasterToPoints(Ghana_map))
prj_dd <-Ghana_map@crs@projargs

site2<-result[,c("Longitude","Latitude","elv")]

centroid_spdf <- SpatialPointsDataFrame(
  site2[,1:2], proj4string=Ghana_map@crs, site2)
cent_max <- raster::extract(ppfd_map,             # raster layer
                            centroid_spdf,   # SPDF with centroids for buffer
                            buffer = 1,     # buffer size, units depend on CRS
                            fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

result$ppfd<-cent_max$layer

# I have also downloaded data from ERA5-land using google earth engine

ERA5_ppfd<-read.csv('Ghana_Amazon_radiation_ERA5_land.csv')%>%
  mutate( # extract cloud state, 00 denote no cloud
    month = substr(date,6,7),
    # extract month from the 6th and 7th digits
    SITE = substr(name,1,3))%>%
  # extract site from the 1st to 3rd digits
  group_by(SITE)%>%
  summarise(shortwave_radiation = mean(surface_solar_radiation_downwards_sum)/30/24/3600/ 1000000 *3600*24 *0.45*J_to_mol )%>%
  filter(SITE %in% c('ANK','BOB','KOG'))

result$ppfd<-ERA5_ppfd$shortwave_radiation*30
# from mol/m2/d to mol/m2/month,

save(result,file = "organized_climate_withppfd.rda")
