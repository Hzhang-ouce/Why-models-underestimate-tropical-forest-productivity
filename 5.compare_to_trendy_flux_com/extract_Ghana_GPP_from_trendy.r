library(tidyverse)
library(raster)
setwd('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/Downloaded_trendy_v9')

# Here we try to extract Ghana gpp From models, I am not using for loop but going through each model because I need to check projection etc

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                       get the extract function ready                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is a list of plots
result<-data.frame(matrix(nrow=8,ncol = 0))
result$Latitude=c(6.6910,5.2680,7.2616,-3.95,-12.8309,-16.02,-1.7160,-13.0765)
result$Longitude=c(-1.3389,-2.6955,-1.1501,-73.4333,-69.2705,-62.73,-51.4570,-52.3858) #From Moore 2017
result$name=c('BOB','ANK','KOG','ALP','TAM','KEN','CAX','Tanguro')
site2<-result[,c("Longitude","Latitude","name")]
centroid_spdf <- SpatialPointsDataFrame(
  site2[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), site2)

# extract function, this is a simple extract without buffer
extract_fapar<-function(Ghana_map,centroid_spdf){
  #Ghana_map<-raster(tif_address)
  cent_max <- raster::extract(Ghana_map,             # raster layer
                              centroid_spdf,   # SPDF with centroids for buffer
                              method='simple',
                              df=TRUE)         # return a dataframe? 
  site2$GPP<-rowMeans(cent_max[,2:length(cent_max)])

  return(site2)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  CLASSIC                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


my_raster<-raster::brick('CLASSIC_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3828 
my_raster2<-my_raster[[(3828-119):3828]] 
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'CLASSIC'
dump_table<-Output_points
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  CLM5.0                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('CLM5.0_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-rotate(my_raster[[(last_year-119):last_year]] )
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'CLM5.0'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  DLEM                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('DLEM_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-my_raster[[(last_year-119):last_year]]
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'DLEM'
dump_table<-rbind(dump_table,Output_points)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  IBIS                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('IBIS_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-my_raster[[(last_year-119):last_year]]
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'IBIS'
dump_table<-rbind(dump_table,Output_points)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  ISAM                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('ISAM_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-rotate(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'ISAM'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  ISBA-CTRIP                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('ISBA-CTRIP_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'ISBA-CTRIP'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  JSBACH_S2_gpp                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('JSBACH_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'JSBACH'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  JULES                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('JULES-ES-1p0_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-rotate(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'JULES'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  LPJ_S2_gpp                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('LPJ_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1, this is not for LPJ, but I think the coder just get the unit wrong
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'LPJ'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  LPJ-GUESS                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



my_raster<-raster::brick('LPJ-GUESS_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'LPJ-GUESS'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  LPX-Bern                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('LPX-Bern_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'LPX-Bern'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  OCN                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('OCN_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'OCN'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  ORCHIDEE                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('ORCHIDEE_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'ORCHIDEE'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  ORCHIDEE-CNP                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# my_raster<-raster::brick('ORCHIDEE-CNP_S2_gpp.nc')
# my_raster@crs
# # Check the CRS is WGS84 or CRS('+init=EPSG:4326')
# Time_series<-my_raster@data@names
# # Grab the time series
# my_raster@data@unit
# # Make sure unit is kgC m-2 s-1
# dim(my_raster)
# # check length is 3840 
# last_year<-dim(my_raster)[3]
# my_raster2<-(my_raster[[(last_year-119):last_year]])
# # we only want the last 10 years
# plot(my_raster2[[1]])
# points(centroid_spdf)
# # plot it out to make sure the points are at the right location
# Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
# Output_points$model<-'ORCHIDEE-CNP'
# dump_table<-rbind(dump_table,Output_points)

# However,

# The above gave a strange ANK value probably because ANK is by the sea, anyway, we take ORCHIDEE-CNP GPP from per pft value

nc_data <- nc_open('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/Downloaded_trendy_v9/gpppft/ORCHIDEE-CNP_S2_gpppft.nc')
print(nc_data)
nc_close(nc_data) 

my_raster<-raster::brick('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/Downloaded_trendy_v9/gpppft/ORCHIDEE-CNP_S2_gpppft.nc',level=2)
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names

dim(my_raster)
# check length is 3840 , ok it is 1440
# we only want the last 10 years
plot(my_raster[[1]])
points(centroid_spdf)
temp_list<-list()
for (i in 1:15) {
  my_raster<-raster::brick('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/Downloaded_trendy_v9/gpppft/ORCHIDEE-CNP_S2_gpppft.nc',level=i)
  my_raster<-(my_raster[[(3828-119):3828]])
  temp_list[[i]]<-extract_fapar(my_raster,centroid_spdf) # extract
  temp_list[[i]]$level<-i
  print(i)
}
Output_points = do.call(rbind, temp_list)
Output_points = do.call(rbind, temp_list)%>%
  group_by(name)%>%
  summarise(GPP = sum(GPP))
Output_points$model<-'ORCHIDEE-CNP'

dump_table<-dplyr::bind_rows(dump_table,Output_points)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  ORCHIDEEv3                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('ORCHIDEEv3_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'ORCHIDEEv3'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  SDGVM                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('SDGVM_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 , ok it is 1440
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'SDGVM'
dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  visit                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_raster<-raster::brick('VISIT_S2_gpp.nc')
my_raster@crs
# Check the CRS is WGS84 or CRS('+init=EPSG:4326')
Time_series<-my_raster@data@names
# Grab the time series
my_raster@data@unit
# Make sure unit is kgC m-2 s-1
dim(my_raster)
# check length is 3840 , ok it is 1920, it says month from 1860, so 160 years in total
last_year<-dim(my_raster)[3]
my_raster2<-(my_raster[[(last_year-119):last_year]])
# we only want the last 10 years
plot(my_raster2[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
Output_points<-extract_fapar(my_raster2,centroid_spdf) # extract
Output_points$model<-'VISIT'
dump_table<-rbind(dump_table,Output_points)

#nc_have_a_look<-nc_open('VISIT_S2_gpp.nc')



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  FLUXCOM                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ncdf4)
library(raster)
ncnew<-nc_open('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/fluxcom/GPP.RS_METEO.FP-ALL.MLM-ALL.METEO-ALL.720_360.monthly.2010.nc')
output<-ncvar_get(ncnew,'GPP')
# Have a look at the nc file
nclist<-list.files(path='H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/fluxcom/',full.names = T)
my_raster <- lapply(nclist,function(x) stack(x,varname="GPP"))

my_raster[[1]]@crs
# check crs
plot(my_raster[[1]])
points(centroid_spdf)
# plot it out to make sure the points are at the right location
my_raster<-brick(my_raster)
Output_points<-extract_fapar(my_raster,centroid_spdf) # extract
Output_points$GPP<-Output_points$GPP /1000 /24/3600
# unit is "gC m-2 d-1" we need to convert it into kgC m-2 s-1

Output_points$model<-'FLUXCOM'

dump_table<-rbind(dump_table,Output_points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ uncertainty of fluxcom  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_raster <- lapply(nclist,function(x) stack(x,varname="GPP_mad"))
plot(my_raster[[1]])
my_raster<-brick(my_raster)
Output_points<-extract_fapar(my_raster,centroid_spdf) # extract
Output_points$GPP<-Output_points$GPP /1000 /24/3600*0.001*10000*3600*24*365


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ unit convert and save  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


dump_table$GPP_MgC_ha_year<-dump_table$GPP*0.001*10000*3600*24*365 # from KgC m2 s-1 to Mgc ha-1 year-1

openxlsx::write.xlsx(dump_table,file='Extract_GEM_from_Trendy_fluxcom.xlsx')

stop('I have saved the data without drawing new figurs')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Draw a figure                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#...............................................................................
#                                                                              .
#  compare trendy models to MODIS FLUXCOM and P-model                          .
#                                                                              .
#...............................................................................



library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)
setwd('F:/Oxford/Chapter_three/github_version/5.compare_to_trendy_flux_com/')
dump_table2<-openxlsx::read.xlsx('Extract_GEM_from_Trendy_fluxcom.xlsx')


trendy_mean<-dump_table2%>%
  filter(model!='FLUXCOM')%>%
  group_by(name)%>%
  summarise( GPP_MgC_ha_year_sd = sd(GPP_MgC_ha_year,na.rm=T),
             GPP_MgC_ha_year=mean(GPP_MgC_ha_year,na.rm=T))%>%
  mutate(model = 'TRENDY_mean')


# Read in field Ghana data and calculate GPP
ghana_fGPP <- read.csv('../3.supplementary_compare_climate_variable/all_GPP_together_per_SITE_20221122.csv')%>%
  dplyr::select(GPP,GPP_se)

# Read in MODIS GPP (Xiongjie downloaded this for me with GEE)
Modis_GPP <- read.csv('GPP_from_MODIS_GPP_Amazonia.csv')%>%
  dplyr::select(Gpp,name,date,Psn_QC)%>%
  filter(Gpp>0)%>% #REMOVE bad data
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(Psn_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)))%>%
  filter(CLOUDSTATE=='0' | CLOUDSTATE=='10')%>%
  mutate(SITE = substr(name,1,3))%>%
  group_by(SITE)%>%
  summarise(GPP=mean(Gpp) /8 *365 /1000/10000*10000)%>% # 20 years average, from kgc/m2/8day to MgC/ha/year, the *10000 is because MODIS gpp was recorded with 1000 scale to save disk space (see modis document)
  dplyr::rename(name=SITE, GPP_MgC_ha_year=GPP)%>%
  mutate(model = 'MODIS')

# Modis_GPP2 <- read.csv('H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/PlotPointsDataGPP_amazonia_xiongjie_MOD17A3HGF_gpp.csv')%>%
#   dplyr::select(Gpp,name,date)%>%
#   mutate(year=substr(date,1,4))%>%
#   filter(Gpp>0)%>% #REMOVE bad data
#   mutate(SITE = substr(name,1,3))%>%
#   group_by(SITE,year)%>%
#   summarise(GPP=mean(Gpp)/1000/10000*10000)%>% # 20 years average, from kgc/m2/year to MgC/ha/year, the *10000 is because MODIS gpp was recorded with 1000 scale to save disk space (see modis document)
#   dplyr::rename(name=SITE, GPP_MgC_ha_year=GPP)%>%
#   mutate(model = 'MODIS')

# Trait-based_GPP, P-model informed by Vcmax and Jmax field measured 
Field_measurement_based_GPP<-openxlsx::read.xlsx( '../4.Figure_two_GPP_from_measured_Vcmax/2023-03-15_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')%>%
  dplyr::select(SITE,gpp_calculated)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_PfL')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp_calculated )

Pmodel_S<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_PPFD.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel(Satellite)')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

Pmodel_F<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment P_model_fAPAR.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel(Field)')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

Pmodel_online<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment Pmodel_02023-03-15.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_null')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )


dump_table3<-plyr::rbind.fill(dump_table2,Modis_GPP,Pmodel_online)



Order_of_model <- c('CLASSIC','IBIS','LPJ','ORCHIDEE','VISIT','ISBA-CTRIP', # these are non- N coupled models
                    'CLM5.0',
                    'DLEM',
                    'ISAM',
                    'JSBACH',
                    'JULES',
                    'LPJ-GUESS',
                    'LPX-Bern',
                    'OCN',
                    'ORCHIDEE-CNP',
                    'ORCHIDEEv3',
                    'SDGVM',
                    'FLUXCOM','MODIS','Pmodel_null')

cls <- data.frame(model=Order_of_model, colour=c(rep('C only',6),rep('CN models',11),'FLUXCOM','MODIS',rep('P-models',1)))
pattern = c(rep('none',17),'stripe','crosshatch')
temp_table<-dump_table3%>%
  filter(name=='ANK')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)

temp_table$model <- factor(temp_table$model, levels = Order_of_model)


ANK<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity", aes(x=model, y=GPP_MgC_ha_year,fill=colour),colour='black', alpha=.6, width=.4,linewidth=0.2) + #models bar plot
  xlab("Models") + 
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[1,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                 ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+ # Field in situ measurements
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[1]), linetype='dotted', col = 'red',linewidth=1)+  # in-situ measurements 
  geom_hline(yintercept=trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='ANK'], linetype='dotted', col = 'purple',linewidth=1)+ # models average
  
  annotate("text", x = temp_table$model[7], y = ghana_fGPP$GPP[1]+2, label = "Mean of field measurements", hjust = "inward")+
  #annotate("text", x = temp_table$model[7], y = trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='ANK']+2, label = "Mean of TRENDY Models", hjust = "inward")+
  theme(legend.position="none")+
  scale_fill_manual(values=c( "#7b3294", "#c2a5cf" ,"#f7f7f7", "#008837",'black'), 
                    name="Data sources")+ggtitle('(a) Site ANK')+ylim(0,47)


temp_table<-dump_table3%>%
  filter(name=='BOB')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)
temp_table$model <- factor(temp_table$model, levels = Order_of_model)

BOB<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity",aes(x=model, y=GPP_MgC_ha_year,fill=colour),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Models") +
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[2,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[2]), linetype='dotted', col = 'red',size=1)+  # in-situ measurements 
  geom_hline(yintercept=trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='BOB'], linetype='dotted', col = 'purple',size=1)+ # models average
  
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[2]+2, label = "Mean of field measurements", hjust = "inward")+
  #annotate("text", x = temp_table$model[1], y = trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='BOB']+2, label = "Mean of TRENDY Models", hjust = "inward")+
  
  theme(legend.position="none")+
  scale_fill_manual(values=c( "#7b3294", "#c2a5cf" ,"#f7f7f7", "#008837",'black'), 
                    name="Data sources")+ggtitle('(b) Site BOB')+ylim(0,47)

temp_table<-dump_table3%>%
  filter(name=='KOG')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)
temp_table$model <- factor(temp_table$model, levels = Order_of_model)

KOG<-temp_table%>%
  ggplot( aes(x=model, y=GPP_MgC_ha_year)) +
  geom_bar(stat="identity",aes(x=model, y=GPP_MgC_ha_year, fill=colour),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Models") +
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[3,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[3]), linetype='dotted', col = 'red',size=1)+  # in-situ measurements 
  geom_hline(yintercept=trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='KOG'], linetype='dotted', col = 'purple',size=1)+ # models average
  
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[3]+2, label = "Mean of field measurements", hjust = "inward")+
  #annotate("text", x = temp_table$model[1], y = trendy_mean$GPP_MgC_ha_year[trendy_mean$name=='KOG']+2, label = "Mean of TRENDY Models", hjust = "inward")+
  
  theme(legend.position="right")+
  scale_fill_manual(values=c( "#7b3294", "#c2a5cf" ,"#f7f7f7", "#008837",'black'), 
                    name="Data sources")+ggtitle('(c) Site KOG')+ylim(0,47)

ggsave(ANK, file='ANK_data_model_compare2.jpg', width=3.42, height=4.18)
ggsave(BOB, file='BOB_data_model_compare2.jpg', width=3.42, height=4.18)
ggsave(KOG, file='KOG_data_model_compare2.jpg', width=4.23, height=4.18)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Draw a figure2                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#...............................................................................
#                                                                              .
#  no trendy models                          .
#                                                                              .
#...............................................................................


library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)
setwd('F:/Oxford/Chapter_three/github_version/5.compare_to_trendy_flux_com/')
dump_table_trendy<-openxlsx::read.xlsx('Extract_GEM_from_Trendy_fluxcom.xlsx')%>%
  filter(model!='FLUXCOM')%>%
  group_by(name)%>%
  summarise( GPP_MgC_ha_year_sd = sd(GPP_MgC_ha_year,na.rm=T),
    GPP_MgC_ha_year=mean(GPP_MgC_ha_year,na.rm=T))%>%
  mutate(model = 'TRENDY_mean')

# Read in field Ghana data and calculate GPP
ghana_fGPP <- read.csv('F:/Oxford/Chapter_two/wirting_up/Ghana_aridity_transect_Carbon_cycle_/input_data/all_GPP_together_per_SITE_20221122.csv')%>%
  dplyr::select(GPP,GPP_se)

# Read in MODIS GPP (Xiongjie downloaded this for me with GEE)
Modis_GPP <- read.csv('GPP_from_MODIS_GPP_Amazonia.csv')%>%
  dplyr::select(Gpp,name,date,Psn_QC)%>%
  filter(Gpp>0)%>% #REMOVE bad data
  mutate(SITE = substr(name,1,3))%>%
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(Psn_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)))%>%
  filter(CLOUDSTATE=='0' | CLOUDSTATE=='10')%>%
  group_by(SITE)%>%
  summarise(GPP=mean(Gpp) /8 *365 /1000/10000*10000)%>% # 20 years average, from kgc/m2/8day to MgC/ha/year, the *1000 is because MODIS gpp was recorded with 1000 scale to save disk space (see modis document)
  dplyr::rename(name=SITE, GPP_MgC_ha_year=GPP)%>%
  mutate(model = 'MODIS')

# Trait-based_GPP, P-model informed by Vcmax and Jmax field measured 
Field_measurement_based_GPP<-openxlsx::read.xlsx( '../4.Figure_two_GPP_from_measured_Vcmax/2023-03-15_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')%>%
  dplyr::select(SITE,gpp_calculated)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_PfL')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp_calculated )

Pmodel_S<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_PPFD.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_P')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

Pmodel_F<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment P_model_fAPAR.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_Pf')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )



dump_table3<-plyr::rbind.fill(Modis_GPP,Field_measurement_based_GPP,Pmodel_F,Pmodel_S)


Order_of_model <- c('in-situ','TRENDY_mean','CLASSIC','IBIS','LPJ','ORCHIDEE','VISIT','ISBA-CTRIP', # these are non- N coupled models
                    'CLM5.0',
                    'DLEM',
                    'ISAM',
                    'JSBACH',
                    'JULES',
                    'LPJ-GUESS',
                    'LPX-Bern',
                    'OCN',
                    'ORCHIDEE-CNP',
                    'ORCHIDEEv3',
                    'SDGVM',
                    'FLUXCOM','MODIS','Pmodel_P','Pmodel_Pf','Pmodel_PfL')

cls <- data.frame(model=Order_of_model, colour=c(rep('C only',8),rep('CN models',11),'FLUXCOM',rep('P-models',3),'MODIS'))
pattern = c(rep('none',17),'stripe','crosshatch')

##~~~~~~~~~~~~~
##  ~ ANK  ----
##~~~~~~~~~~~~~

temp_table<-dump_table3%>%
  filter(name=='ANK')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)

temp_table$model <- factor(temp_table$model, levels = Order_of_model)


ANK<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity", aes(x=model, y=GPP_MgC_ha_year,fill=model),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Experiments") + 
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[1,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+ # Field in situ measurements
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[1]), linetype='solid', col = '#e78ac3',size=1)+  # in-situ measurements 
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[1]+2, label = "Biometric", hjust = "inward",size=3.5)+
  scale_fill_manual(values=c(  "#ffff99" ,"#fdc086", "#386cb0",'#7fc97f'), 
                    name="Data sources")+
  ggtitle('Site: ANK')+
  theme(legend.position="none",plot.title = element_text(size = 10))+ylim(0,47)

stack_percetnage_plot<-ghana_fGPP[1,]%>%
  dplyr::rename(GPP_MgC_ha_year=GPP)%>%
  mutate(model='in-situ')%>%
  plyr::rbind.fill(temp_table)%>%
  filter(model!='TRENDY_mean')%>%
  mutate(name='ANK')%>%
  arrange(desc(GPP_MgC_ha_year))%>%
  mutate(model=factor(model, levels = Order_of_model))
stack_percetnage_plot$percentage[1]<-(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[2])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[2]<-(stack_percetnage_plot$GPP_MgC_ha_year[2]-stack_percetnage_plot$GPP_MgC_ha_year[3])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[3]<-(stack_percetnage_plot$GPP_MgC_ha_year[3]-stack_percetnage_plot$GPP_MgC_ha_year[4])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[4]<-(stack_percetnage_plot$GPP_MgC_ha_year[4]-stack_percetnage_plot$GPP_MgC_ha_year[5])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
stack_percetnage_plot$percentage<-percent(stack_percetnage_plot$percentage,digits=0)
stack_percetnage_plot$percentage[5]<-''


stack_figure <- ggplot(stack_percetnage_plot) +
  geom_bar(aes(x=' ', y=GPP_MgC_ha_year, fill = model),stat="identity",position = "identity") +
 #   geom_text(aes(x=name,y=GPP_MgC_ha_year-3,label=percentage),size = 3, stat="identity",position = "identity")+
  scale_fill_manual(values=c("#e78ac3","#7fc97f", "#386cb0" ,"#ffff99", "#ffff99"), 
                    name="Data-model discrepency \nresolved by",
                    labels=c("unresolved discrepency","field trait" ,"field fapar", "optimality trait", " "))+
  theme_bw()+
  ylab('')+xlab('Diff')+
  theme(legend.position="none")+ylim(0,47)

my_filnal_output<-egg::ggarrange(ANK,stack_figure,ncol=2, widths = c(10,1))
ggsave(my_filnal_output,width=2,height=3.48,filename = 'ANK_data-model_descrepency_partitioning_V2no-cloud.jpg')

##~~~~~~~~~~~~~
##  ~ BOB  ----
##~~~~~~~~~~~~~

temp_table<-dump_table3%>%
  filter(name=='BOB')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)

temp_table$model <- factor(temp_table$model, levels = Order_of_model)



BOB<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity", aes(x=model, y=GPP_MgC_ha_year,fill=model),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Experiments") + 
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[2,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+ # Field in situ measurements
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[2]), linetype='solid', col = '#e78ac3',size=1)+  # in-situ measurements 
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[2]+2, label = "Biometric", hjust = "inward",size=3.5)+
  scale_fill_manual(values=c(  "#ffff99" ,"#fdc086", "#386cb0",'#7fc97f'), 
                    name="Data sources")+
  ggtitle('Site: BOB')+
  theme(legend.position="none",plot.title = element_text(size = 10))+ylim(0,47)

stack_percetnage_plot<-ghana_fGPP[2,]%>%
  dplyr::rename(GPP_MgC_ha_year=GPP)%>%
  mutate(model='in-situ')%>%
  plyr::rbind.fill(temp_table)%>%
  filter(model!='TRENDY_mean')%>%
  mutate(name='BOB')%>%
  arrange(desc(GPP_MgC_ha_year))%>%
  mutate(model=factor(model, levels = Order_of_model))
stack_percetnage_plot$percentage[1]<-(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[2])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[2]<-(stack_percetnage_plot$GPP_MgC_ha_year[2]-stack_percetnage_plot$GPP_MgC_ha_year[3])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[3]<-(stack_percetnage_plot$GPP_MgC_ha_year[3]-stack_percetnage_plot$GPP_MgC_ha_year[4])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[4]<-(stack_percetnage_plot$GPP_MgC_ha_year[4]-stack_percetnage_plot$GPP_MgC_ha_year[5])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
stack_percetnage_plot$percentage<-percent(stack_percetnage_plot$percentage,digits=0)
stack_percetnage_plot$percentage[5]<-''


stack_figure <- ggplot(stack_percetnage_plot) +
  geom_bar(aes(x=' ', y=GPP_MgC_ha_year, fill = model),stat="identity",position = "identity") +
  #   geom_text(aes(x=name,y=GPP_MgC_ha_year-3,label=percentage),size = 3, stat="identity",position = "identity")+
  scale_fill_manual(values=c("#e78ac3","#7fc97f", "#386cb0" ,"#fdc086", "#ffff99"), 
                    name="Data-model discrepency \nresolved by",
                    labels=c("unresolved discrepency","field trait" ,"field fapar", "optimality trait", " "))+
  theme_bw()+
   ylab('')+xlab('Diff')+
  theme(legend.position="none")+ylim(0,47)

my_filnal_output<-egg::ggarrange(BOB,stack_figure,ncol=2, widths = c(10,1))
ggsave(my_filnal_output,width=2,height=3.48,filename = 'BOB_data-model_descrepency_partitioning_V2no-cloud.jpg')


##~~~~~~~~~~~~~
##  ~ KOG  ----
##~~~~~~~~~~~~~


temp_table<-dump_table3%>%
  filter(name=='KOG')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)

temp_table$model <- factor(temp_table$model, levels = Order_of_model)


KOG<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity", aes(x=model, y=GPP_MgC_ha_year,fill=model),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Experiments") + 
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[3,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+ # Field in situ measurements
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[3]), linetype='solid', col = '#e78ac3',size=1)+  # in-situ measurements 
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[3]+2, label = "Biometric", hjust = "inward",size=3.5)+

  scale_fill_manual(values=c(  "#ffff99" ,"#fdc086", "#386cb0",'#7fc97f'), 
                    name="Data sources")+
  ylim(c(0,47))+ggtitle('Site: KOG')+
  theme(legend.position="none",plot.title = element_text(size = 10))

stack_percetnage_plot<-ghana_fGPP[3,]%>%
  dplyr::rename(GPP_MgC_ha_year=GPP)%>%
  mutate(model='in-situ')%>%
  plyr::rbind.fill(temp_table)%>%
  filter(model!='TRENDY_mean')%>%
  mutate(name='KOG',
         model=factor(model,levels = Order_of_model))%>%
  arrange(model)%>%
  mutate(model=factor(model, levels = Order_of_model))
stack_percetnage_plot$percentage[1]<-(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[2])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[2]<-(stack_percetnage_plot$GPP_MgC_ha_year[2]-stack_percetnage_plot$GPP_MgC_ha_year[3])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[3]<-(stack_percetnage_plot$GPP_MgC_ha_year[3]-stack_percetnage_plot$GPP_MgC_ha_year[4])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[4]<-(stack_percetnage_plot$GPP_MgC_ha_year[4]-stack_percetnage_plot$GPP_MgC_ha_year[5])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
stack_percetnage_plot$percentage<-percent(stack_percetnage_plot$percentage,digits=0)
stack_percetnage_plot$percentage[5]<-''


stack_figure <- ggplot(stack_percetnage_plot) +
  geom_bar(aes(x=' ', y=GPP_MgC_ha_year, fill = model),stat="identity",position = "identity") +
  #   geom_text(aes(x=name,y=GPP_MgC_ha_year-3,label=percentage),size = 3, stat="identity",position = "identity")+
  scale_fill_manual(values=c("#e78ac3","#7fc97f", "#386cb0" ,"#fdc086", "#ffff99"), 
                    name="Data-model discrepency \nresolved by",
                    labels=c("unresolved discrepency","field-collected traits" ,"field-observed fAPAR", "trait optimisation", " "))+
  theme_bw()+
  ylim(c(0,47))+ylab('')+xlab('Diff')

my_filnal_output<-egg::ggarrange(KOG,stack_figure,ncol=2, widths = c(10,1))
ggsave(my_filnal_output,width=4,height=3.48,filename = 'KOG_data-model_descrepency_partitioning_V2no-cloud.jpg')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            figure for Amazonia                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Order_of_site<- c('ALP','ANK','CAX','TAM','BOB', 'KEN','KOG', 'Tanguro')
#Order_of_site<- c('ANK','BOB','KOG','ALP','TAM', 'KEN','CAX', 'Tanguro')
Area<-c('Amazonia','West Africa','Amazonia','Amazonia','West Africa','Amazonia','West Africa', 'Amazonia')
site_table<-cbind.data.frame(Order_of_site,Area)%>%dplyr::rename(name=Order_of_site)

dump_table_trendy<-openxlsx::read.xlsx('Extract_GEM_from_Trendy_fluxcom.xlsx')%>%
  filter(model!='FLUXCOM')%>%
  group_by(name)%>%
  summarise(GPP_MgC_ha_year=mean(GPP_MgC_ha_year,na.rm=T))%>%
  mutate(model = 'TRENDY_mean')

# Read in field Ghana data and calculate GPP
ghana_fGPP <- read.csv('H:/Oxford/Chapter_two/wirting_up/Ghana_aridity_transect_Carbon_cycle_/input_data/all_GPP_together_per_SITE_20221122.csv')%>%
  dplyr::select(GPP,GPP_se)
amazon_fGPP <- read.csv('H:/Oxford/Chapter_two/wirting_up/Ghana_aridity_transect_Carbon_cycle_/input_data/MCWD.csv')%>%
  mutate(GPP_se=GPP_max-GPP_mean)

amazon_fGPP$name <- c('ALP','ALP','TAM','TAM', 'KEN','KEN','CAX','CAX', 'Tanguro', 'Tanguro')
amazon_fGPP<-amazon_fGPP%>%group_by(name)%>%
  summarise(GPP=mean(GPP_mean),
            GPP_se=mean(GPP_se))
ghana_fGPP$name<-c('ANK','BOB', 'KOG' )
fieldGPP<-plyr::rbind.fill(ghana_fGPP,amazon_fGPP)%>%
  mutate(model='in-situ')%>%
  rename(GPP_MgC_ha_year=GPP)

dump_table2<-openxlsx::read.xlsx('Extract_GEM_from_Trendy_fluxcom.xlsx')%>%
  filter(model=='FLUXCOM')
dump_table3<-plyr::rbind.fill(dump_table2,Modis_GPP,fieldGPP,dump_table_trendy)
dump_table3$name[16]<-'Tanguro'
dump_table3<-left_join(dump_table3,site_table)
dump_table3$name <- factor(dump_table3$name, levels = Order_of_site)
  

bymysite<-ggplot(dump_table3, aes(x = factor(name), y = GPP_MgC_ha_year,  fill = model)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9)+
  labs( x = 'Rank by MCWD (From Wet to Dry)', y = 'GPP_MgC_ha_year', fill = "Method")+
  theme(legend.position="bottom",text = element_text(size=12)) +
  theme_bw()
ggsave(bymysite,filename = 'H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/site_modis_fluxcom_compare.jpg',width=7.61,height=3.80)

GPP <- ggplot(dump_table3%>%filter(model!='TRENDY_mean'),aes(name, GPP_MgC_ha_year, colour=Area, group = Area)) +
  geom_point(size=3) +
  geom_line()+
  theme_bw() +
  labs(x='Rank by MCWD (From Wet to Dry)', 
       y="GPP (Mg C ha-1 yr-1)") +
  guides(size="none") +
  facet_wrap(~model)+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) 
ggsave(GPP,filename = 'H:/Oxford/Chapter_three/Compare_to_Trend_andMODIS/Ghana_Amazon_compare.jpg',width=7.40,height=3.35)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            figure for Yadvinder ESA talk                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)
setwd('H:/Oxford/Chapter_three/github_version/5.compare_to_trendy_flux_com/')
dump_table_trendy<-openxlsx::read.xlsx('Extract_GEM_from_Trendy_fluxcom.xlsx')%>%
  filter(model!='FLUXCOM')%>%
  group_by(name)%>%
  summarise( GPP_MgC_ha_year_sd = sd(GPP_MgC_ha_year,na.rm=T),
             GPP_MgC_ha_year=mean(GPP_MgC_ha_year,na.rm=T))%>%
  mutate(model = 'TRENDY_mean')

# Read in field Ghana data and calculate GPP
ghana_fGPP <- read.csv('H:/Oxford/Chapter_two/wirting_up/Ghana_aridity_transect_Carbon_cycle_/input_data/all_GPP_together_per_SITE_20221122.csv')%>%
  dplyr::select(GPP,GPP_se)

# Read in MODIS GPP (Xiongjie downloaded this for me with GEE)
Modis_GPP <- read.csv('GPP_from_MODIS_GPP_Amazonia.csv')%>%
  dplyr::select(Gpp,name,date,Psn_QC)%>%
  filter(Gpp>0)%>% #REMOVE bad data
  mutate(SITE = substr(name,1,3))%>%
  rowwise()%>%
  mutate( FparQC_binary=paste(rev(as.integer(intToBits(Psn_QC))), collapse=""),
          # convert to binary
          FparQC_binary = stringi::stri_sub(FparQC_binary,-8,-1),
          # only last 8 digits are meaningful, remove other 0 at the start of the string
          CLOUDSTATE = as.numeric(substr(FparQC_binary,4,5)))%>%
  filter(CLOUDSTATE=='0' | CLOUDSTATE=='10')%>%
  group_by(SITE)%>%
  summarise(GPP=mean(Gpp) /8 *365 /1000/10000*10000)%>% # 20 years average, from kgc/m2/8day to MgC/ha/year, the *1000 is because MODIS gpp was recorded with 1000 scale to save disk space (see modis document)
  dplyr::rename(name=SITE, GPP_MgC_ha_year=GPP)%>%
  mutate(model = 'MODIS')

# Trait-based_GPP, P-model informed by Vcmax and Jmax field measured 
Field_measurement_based_GPP<-openxlsx::read.xlsx( '../4.Figure_two_GPP_from_measured_Vcmax/2023-03-15_plug_in_vcmax_jmax_and_get_Pmodel_output.xlsx')%>%
  dplyr::select(SITE,gpp_calculated)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_PfL')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp_calculated )

Pmodel_S<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment_Pmodel_PPFD.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_P')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

Pmodel_F<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment P_model_fAPAR.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_Pf')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

Pmodel_online<-openxlsx::read.xlsx('../4.Figure_two_GPP_from_measured_Vcmax/Experiment Pmodel_02023-03-15.xlsx')%>%
  dplyr::select(SITE,gpp)%>%
  mutate(SITE=substr(SITE,1,3),model='Pmodel_null')%>%
  rename(name=SITE,GPP_MgC_ha_year =gpp )

dump_table3<-plyr::rbind.fill(Modis_GPP,Field_measurement_based_GPP,Pmodel_F,Pmodel_S,Pmodel_online)


Order_of_model <- c('in-situ','TRENDY_mean','CLASSIC','IBIS','LPJ','ORCHIDEE','VISIT','ISBA-CTRIP', # these are non- N coupled models
                    'CLM5.0',
                    'DLEM',
                    'ISAM',
                    'JSBACH',
                    'JULES',
                    'LPJ-GUESS',
                    'LPX-Bern',
                    'OCN',
                    'ORCHIDEE-CNP',
                    'ORCHIDEEv3',
                    'SDGVM',
                    'FLUXCOM','Pmodel_PfL','Pmodel_Pf','Pmodel_P','Pmodel_null','MODIS')

cls <- data.frame(model=Order_of_model, colour=c(rep('C only',8),rep('CN models',11),'FLUXCOM',rep('P-models',4),'MODIS'))
pattern = c(rep('none',17),'stripe','crosshatch')



temp_table<-dump_table3%>%
  filter(name=='BOB')%>%
  left_join(., cls, by = "model")%>%
  drop_na(GPP_MgC_ha_year)

temp_table$model <- factor(temp_table$model, levels = Order_of_model)



BOB<-temp_table%>%
  ggplot( ) +
  geom_bar(stat="identity", aes(x=model, y=GPP_MgC_ha_year,fill=model),colour='black', alpha=.6, width=.4,size=0.2) + #models bar plot
  xlab("Experiments") + 
  ylab("GPP (MgC/ha/yr)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  geom_rect(data=ghana_fGPP[2,], inherit.aes=FALSE, aes(xmin=-Inf, xmax=Inf, ymin=min(GPP-GPP_se),
                                                        ymax=max(GPP+GPP_se)), color="transparent", fill="grey", alpha=0.3)+ # Field in situ measurements
  geom_hline(yintercept=as.numeric(ghana_fGPP$GPP[2]), linetype='solid', col = '#e78ac3',size=1)+  # in-situ measurements 
  annotate("text", x = temp_table$model[1], y = ghana_fGPP$GPP[2]+2, label = "In-situ GPP", hjust = "inward",size=3.5)+
  scale_fill_manual(values=c(  "#7fc97f" ,"#386cb0", "#fdc086",'#FFFFFF','#FFFFFF'), 
                    name="Data sources")+
  ggtitle('Site: BOB')+
  theme(legend.position="none",plot.title = element_text(size = 10))+ylim(0,47)

temp_table<-temp_table%>%
  filter(model!='Pmodel_null') # we need to remove Pmodel_null from stacked bar plot

stack_percetnage_plot<-ghana_fGPP[2,]%>%
  dplyr::rename(GPP_MgC_ha_year=GPP)%>%
  mutate(model='in-situ')%>%
  plyr::rbind.fill(temp_table)%>%
  filter(model!='TRENDY_mean')%>%
  mutate(name='BOB')%>%
  arrange(desc(GPP_MgC_ha_year))%>%
  mutate(model=factor(model, levels = Order_of_model))
stack_percetnage_plot$percentage[1]<-(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[2])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[2]<-(stack_percetnage_plot$GPP_MgC_ha_year[2]-stack_percetnage_plot$GPP_MgC_ha_year[3])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[3]<-(stack_percetnage_plot$GPP_MgC_ha_year[3]-stack_percetnage_plot$GPP_MgC_ha_year[4])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])
stack_percetnage_plot$percentage[4]<-(stack_percetnage_plot$GPP_MgC_ha_year[4]-stack_percetnage_plot$GPP_MgC_ha_year[5])/(stack_percetnage_plot$GPP_MgC_ha_year[1]-stack_percetnage_plot$GPP_MgC_ha_year[5])

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
stack_percetnage_plot$percentage<-percent(stack_percetnage_plot$percentage,digits=0)
stack_percetnage_plot$percentage[5]<-''


stack_figure <- ggplot(stack_percetnage_plot) +
  geom_bar(aes(x=' ', y=GPP_MgC_ha_year, fill = model),stat="identity",position = "identity") +
  #   geom_text(aes(x=name,y=GPP_MgC_ha_year-3,label=percentage),size = 3, stat="identity",position = "identity")+
  scale_fill_manual(values=c("#e78ac3","#7fc97f", "#386cb0" ,"#fdc086", "#FFFFFF"), 
                    name="Data-model discrepency \nresolved by",
                    labels=c("unresolved discrepency","field trait" ,"field fapar", "optimality trait", " "))+
  theme_bw()+
  ylab('')+xlab('Diff')+
  theme(legend.position="none")+ylim(0,47)

my_filnal_output<-egg::ggarrange(BOB,stack_figure,ncol=2, widths = c(10,1))

