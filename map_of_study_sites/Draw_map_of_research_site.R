 

convert_cloud_state<-function(x){
  
FparQC_binary<-paste(rev(as.integer(intToBits(x))), collapse="")
# convert to binary
FparQC_binary<- stringi::stri_sub(FparQC_binary,-8,-1)
# only last 8 digits are meaningful, remove other 0 at the start of the string
CLOUDSTATE <- as.numeric(substr(FparQC_binary,4,5))
return(CLOUDSTATE)
}
  


library(tidyverse)

setwd('H:/Oxford/Chapter_three/github_version/map_of_study_sites')

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(raster)
library(rasterVis)
library(sp)
library(rgdal)
library(grid)

                    # spatial library

dat_orig <- read.table(text="site  lng  lat   
KOG  -1.14995275  7.262316 

 ALP  -73.4333  -3.95 
 TAM  -69.2705 -12.8309 
 KEN  -62.73 -16.02 

 CAX  -51.4570 -1.7160 

 Tanguro  -52.3858 -13.0765 
 
 BOB  -1.3190675  6.70471325 

 ANK  -2.69364  5.2678675", header=TRUE, stringsAsFactors=FALSE)


dat_point <- SpatialPointsDataFrame(dat_orig[,c("lng", "lat")], dat_orig)

dat_orig <- read.table(text="site  lng  lat   
KOG  -1  7.8 

 ALP  -73.4333  -1.95 
 TAM  -69.2705 -10.8309
 KEN  -62.73 -14.22 

 CAX  -51.4570 0.3 

 Tanguro  -52.3858 -11
 
 BOB  -0.5  6.70471325 

 ANK  -3.5  4.6678675", header=TRUE, stringsAsFactors=FALSE)


dat_text <- SpatialPointsDataFrame(dat_orig[,c("lng", "lat")], dat_orig)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sp')

# grab fapar map instead
brksUniv <- seq(0,100, length.out=20)
mylabel<- seq(0,100, length.out=5)

cols <- c(colorRampPalette(c("white", "black"))(20))
myColorkey <- list( tri.lower = TRUE, tri.upper = TRUE,col = cols, at=brksUniv, labels=list(at=mylabel, labels=mylabel))

cols2 <- c(colorRampPalette(c("pink", "pink"))(20))
cols3 <- c(colorRampPalette(c("white", "white"))(20))

raster_fapar<-raster("H:/Oxford/Chapter_one/modis_fapar/MOD15A2H.006_20200914140559/MOD15A2H.A2016249.Fpar_500m.tif")
raster_fapar<-raster::crop(raster_fapar,extent(-10,10,0,20))
plot(raster_fapar)
raster_fapar[raster_fapar>100]<-NA
Oceanmap<-raster_fapar
Oceanmap[is.na(Oceanmap)]<- 10000
Oceanmap[Oceanmap<10000]<-NA

plot(Oceanmap)
raster_fapar_std<-raster("FaparQC_500m_2016-09-01_MOD15A2H.tif")
raster_fapar_std<-raster::crop(raster_fapar_std,extent(-10,10,0,20))
#raster_fapar_std<-raster::crop(raster_fapar_std,extent(-1,1,1,2))

rasterOptions(tmpdir="C:\\Users/Huanyuan/Documents/", tmptime = 24, progress="text", timer=TRUE,
              overwrite = T, chunksize=8e8, maxmemory=3.5e+10)
raster_fapar_std2<-calc(raster_fapar_std,convert_cloud_state)


plot(raster_fapar_std)
hist(raster_fapar_std)
missing_raster_fapar<-raster_fapar_std2
missing_raster_fapar[missing_raster_fapar==11]<-NA # this is ocean
missing_raster_fapar[missing_raster_fapar==0]<-NA # no cloud
missing_raster_fapar[missing_raster_fapar==10]<-NA # mixed cloud
plot(missing_raster_fapar)


jpeg("africa_map_on_fapar.jpg", width=6.81, height=3.44, res=400, units="in")


    rasterVis::levelplot(xlab='', ylab='',raster_fapar,margin=F ,colorkey=myColorkey, at=brksUniv,col.regions = cols) + 
      rasterVis::levelplot(xlab='', ylab='',missing_raster_fapar,margin=F ,colorkey=myColorkey, at=brksUniv,col.regions = cols2) + 
  latticeExtra::layer(sp.polygons(worldmap))+
  latticeExtra::layer(sp.points(dat_point,pch=19,col="red"))
  #latticeExtra::layer(sp.text(coordinates(dat_text), dat_text$site,col="red"))

trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid ::grid.text('fAPAR (%)', 0.2, -0.1, hjust=0.5, vjust=1.5)
trellis.unfocus()
dev.off()

jpeg("africa_map_on_fapar_zoomin.jpg", width=4.81, height=3.44, res=400, units="in")

raster_fapar<-raster::crop(raster_fapar,extent(-5,3,3,12))

rasterVis::levelplot(xlab='', ylab='',raster_fapar,margin=F ,colorkey=myColorkey, at=brksUniv,col.regions = cols) + 
  rasterVis::levelplot(xlab='', ylab='',missing_raster_fapar,margin=F ,colorkey=myColorkey, at=brksUniv,col.regions = cols2) + 
  latticeExtra::layer(sp.polygons(worldmap))+
  latticeExtra::layer(sp.points(dat_point,pch=19,col="red"))+
  latticeExtra::layer(sp.text(coordinates(dat_text), dat_text$site,col="red"))

trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid ::grid.text('fAPAR (%)', 0.2, -0.1, hjust=0.5, vjust=1.5)
trellis.unfocus()
dev.off()
