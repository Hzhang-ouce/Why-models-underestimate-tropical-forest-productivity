
setwd('H:/Oxford/Chapter_three/convert_nc_to_geotiff')

library(ncdf4)
library(raster)
ex.nc = raster('VISIT_npp_FINAL_MEAN.nc')

plot(ex.nc)
ras_out<-writeRaster(ex.nc,"VISIT_npp_FINAL_MEAN_as_example_for_xiongjie.tif",format="GTiff",overwrite=T)
