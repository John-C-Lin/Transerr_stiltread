#Test Ben Fasoli's implementation of R code to directly read in variables stored in ARL-formatted met files: https://github.com/benfasoli/stiltread/blob/master/README.md
#Limitations
#Returned raster layers are limited to files that use Lambert Conformal Conic for the map projection, which is typical for midlatitudes.
#Upper levels of pressure variables are often recorded as a difference from the base level. This calculation needs to be performed by the user.
#March 11th, 2019 by John C. Lin (John.Lin@utah.edu)

#####################
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group8/hrrr/data/utah/"
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group8/hrrr/data/west/"
#metfile<-"20170617.00z.hrrra"; yy<-17;mm<-6;dd<-17;hh<-3
#metfile<-"20180617.00z.hrrra"; yy<-18;mm<-6;dd<-17;hh<-3
#metfile<-"20190301.00z.hrrra"; yy<-19;mm<-3;dd<-1;hh<-3
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group11/hrrr-gsv-slv/"
#metfile<-"20190808_00-05_hrrr"; yy<-19;mm<-8;dd<-8;hh<-3
metdir<-"/uufs/chpc.utah.edu/common/home/lin-group11/hrrr/"
metfile<-"20190726_00-05_hrrr"; yy<-19;mm<-7;dd<-26;hh<-3
#####################
#devtools::install_github('uataq/stiltread')

library(stiltread)
#install_stiltread()
library(raster)

#returns a raster::RasterLayer containing data, grid coordinates, and projection information using the standard PROJ4 format.
shgt <- read_met(paste0(metdir,metfile),
                 var = 'shgt',
                 yy = yy, mm = mm,
                 dd = dd, hh = hh, lvl = 0)
#saveRDS(shgt,file="shgt.RDS");shgt<-readRDS("shgt.RDS")
#a) Since read_met returns a raster::RasterLayer with projection metadata, we can leverage standard mapping libraries in R.Since read_met returns a raster::RasterLayer with projection metadata, we can leverage standard mapping libraries in R.
newproj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
shgt.latlon <- projectRaster(shgt,crs=newproj)
dev.new();plot(shgt.latlon)
# zoom into Salt Lake Valley
shgt.zoom.latlon <- crop(shgt.latlon,extent(-112.4,-111.4,40.40,41.00))
dev.new();plot(shgt.zoom.latlon)
shgt.zoom <- crop(shgt,extent(-1600000,-1000000,-7E5,5E5))
dev.new();plot(shgt.zoom)
dev.new();plot(shgt)

#b) For an interactive leaflet map,
#library(leaflet)
#leaflet() %>%
#  addProviderTiles('CartoDB.Positron') %>%
#  addRasterImage(shgt, opacity = 0.5, colors = viridis::viridis(16))

#c) if want to use old way to plot (using image.plot)
#shgt<-t(as.matrix(shgt))
#shgt<-shgt[,ncol(shgt):1]
require(fields)
image.plot(shgt,col=topo.colors(64))

#Since wind is grid relative, use read_met_wind instead of read_met to access wind fields. This is called in the same way as read_met, only no var argument is required. This returns a rasterStack with u and v layers that have been rotated into +proj=longlat.
for(nlvl in seq(0,10,2)){
  uv <- read_met_wind(paste0(metdir,metfile),
                    yy = yy, mm = mm,
                    dd = dd, hh = hh, lvl = nlvl)
  dev.new();plot(uv$u,zlim=c(-15,20),main=paste("U; lvl =",nlvl))
  gc()
  #plot(uv$v)
} #for(nlvl in 0:10){

#try just calling read_met to access un-rotated winds 
#u <- read_met(paste0(metdir,metfile),var='UWND',
#                    yy = yy, mm = mm, dd = dd, hh = hh, lvl = 6)
#plot(u)


