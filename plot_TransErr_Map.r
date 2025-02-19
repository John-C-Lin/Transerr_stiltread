# Plot results of comparisons between Obs vs Sim met variables to help quantify transport errors
# Reads in output from 'TransErr_HRRR.r'
# V2(201217): due to Google's new restrictions on API access, plot maps in a different way (https://github.com/markusloecher/rgooglemaps/tree/master/rgooglemaps)
# V3(201228): add WSPD (windspeed) as a variable whose errors can be determined
# V4(210130): 'start'=>'start.file'; 'end'=>'end.file'
# March 15th, 2019 by John C. Lin (John.Lin@utah.edu)

###################
#YYYYMMDDHHmm
#start.file<- '201509010000'; end.file  <- '201510312300' #YYYYMMDDHHmm
#start.file<- '201512010000'; end.file  <- '201602282300' #YYYYMMDDHHmm
#start.file<- '201803010000'; end.file  <- '201805312300' #YYYYMMDDHHmm
#start.file<- '201806010000'; end.file  <- '201808312300' #YYYYMMDDHHmm
#start.file<- '201809010000'; end.file  <- '201811302300' #YYYYMMDDHHmm
#start.file<- '201812010000'; end.file  <- '201902282300' #YYYYMMDDHHmm
#start.file<- '201908010000'; end.file  <- '201909302300' #YYYYMMDDHHmm
#start.file<- '201910010000'; end.file  <- '201911302300' #YYYYMMDDHHmm
#start.file<- '201912010000'; end.file  <- '202002292300' #YYYYMMDDHHmm
# GSV paper case studies
#start.file<- '201908081800'; end.file  <- '201908082100' #Landfill case study for GSV paper
#start.file<- '201908081400'; end.file  <- '201908081700' #Gravel pit case study for GSV paper
#start.file<- '201909091400'; end.file  <- '201909091700' #Gravel pit case study for GSV paper
# JCL(210321): for Uintah Basin analyses, choose whole years between 2016 and 2020 for analyses and save objects on annual timsecale to facilitate analyses later
start.file<-c("201601010000","201701010000","201801010000","201901010000","202001010000","202101010000","202201010000","202301010000","202401010000")[9]
end.file  <-c("201612312300","201712312300","201812310000","201912312300","202012312300","202112312300","202212312300","202312312300","202412312300")[9]

outputdir <- "out/"   #output from 'TransErr_HRRR.r'

mettype<-"HRRR"
VARS<-c("U","V","WSPD","T")
#XLIMS<-NULL;YLIMS<-NULL  #set lat/lon limits for interpolation;  set to NULL if dynamically determine
#a)  Salt Lake Valley domain
#XLIMS<-c(-112.1,-111.7);YLIMS<-c(40.4,41.1)  #set lat/lon limits for limits of map;  set to NULL if dynamically determine
#b)  Uintah Basin domain
XLIMS<-c(-111.05,-108.9);YLIMS<-c(39.4,41.0)  #set lat/lon limits for limits of map;  set to NULL if dynamically determine
#dx<-0.002;dy<-dx  #grid spacing of interpolated error "map"
dx <- 0.004;dy <- dx  #grid spacing of interpolated error "map"

plotgooglemapTF <- TRUE
plotstidTF <- TRUE      #whether or not to plot station code 
###################

library(gstat);library(fields)
require(RgoogleMaps)

objname<-paste0(outputdir,"/",mettype,"_Errstats_",start.file,"to",end.file,".RDS")
DAT<-readRDS(file=objname)
dat<-DAT$dat

station.info <- DAT$station.info
sel <- rownames(dat)%in%intersect(rownames(station.info),rownames(dat))
dat <- dat[sel,] 
sel <- rownames(station.info)%in%intersect(rownames(station.info),rownames(dat))
station.info <- station.info[sel,] 
LON.all <- station.info[,"LON"];LAT.all<-station.info[,"LAT"]

vars<-colnames(dat)
vars<-vars[substring(vars,1,1)%in%substring(VARS,1,1)]
for(i in 1:length(vars)){
  var<-vars[i]
  err<-dat[,var]
  sel<-is.na(err)|is.nan(err)|is.na(LON.all)|is.na(LAT.all)
  err<-err[!sel];LON<-LON.all[!sel];LAT<-LAT.all[!sel]
  station.info.sub<-station.info[!sel,]

#lons/lats covering range
if(is.null(XLIMS[1])|is.null(YLIMS[1])){
  #a) dynamically determine
  lons<-seq(floor(min(LON)),ceiling(max(LON)),dx)
  lats<-seq(floor(min(LAT)),ceiling(max(LAT)),dy)
} else {
  #b) manually set
  lons<-seq(XLIMS[1]-1,XLIMS[2]+1,dx)
  lats<-seq(YLIMS[1],YLIMS[2],dy)
} #if(is.null(XLIMS[1])|is.null(YLIMS[1])){

  #sel.x<-LON>=XLIMS[1]&LON<=XLIMS[2]
  #sel.y<-LAT>=YLIMS[1]&LAT<=YLIMS[2]
  #LON<-LON[sel.x&sel.y];LAT<-LAT[sel.x&sel.y]
  #err<-err[sel.x&sel.y]

  # create matrix of lons and lats--so that get vector of lons and lats that have same length
  lons.mat<-matrix(rep(lons,times=length(lats)),ncol=length(lats))
  lats.mat<-matrix(rep(lats,times=length(lons)),byrow=T,nrow=length(lons))

  # interpolate irregular lat/lon grid onto regular lat/lon grid
  tmp<-data.frame(lon=LON,lat=LAT,err=err)
  #nmax<-floor(length(lons)/10)  
  #nmax<-2   #really local interpolation
  #mat.gstat <- gstat(id="err", formula=err ~ 1, locations=~lon+lat,data = tmp,nmax=nmax)
  maxdist<-1.0   #maxdist: for local kriging: only observations within a distance of
                 #‘maxdist’ from the prediction location are used for prediction or simulation; if combined with ‘nmax’, both #criteria apply
  nmin<-2
  mat.gstat <- gstat(id="err", formula=err ~ 1, locations=~lon+lat,data = tmp,nmin=nmin,maxdist=maxdist)
  mat.grid<-data.frame(lon=as.vector(lons.mat),lat=as.vector(lats.mat))
  z <- predict(mat.gstat, mat.grid)
  zmat<-matrix(z[,3],ncol=length(lats),nrow=length(lons),byrow=F)
  dimnames(zmat)<-list(lons,lats)
if(!plotgooglemapTF){
  xmain<-paste(var,"\n",mettype,start.file,"to",end.file)
  dev.new();image.plot(x=lons,y=lats,z=zmat,zlim=range(err),main=xmain,cex.main=1.3)
  map("state",add=TRUE);map.cities(minpop=100000)
  points(LON,LAT,pch=17,col="black")
  text(LON,LAT,labels=rownames(station.info.sub))
  #err.interp<-zmat

  #convert to raster format
  #require(raster)
  #zras<-raster(zmat,xmn=lons[1],xmx=max(lons),ymn=lats[1],ymx=max(lats),crs='+proj=longlat')
  #plot(zras)
  #map("state",add=TRUE);map.cities(minpop=100000)
  #points(LON,LAT,pch=17,col="black")

} else{

  xmain<-paste(mettype,"vs MesoWest:   ",var)
  xsub<-paste(start.file,"to",end.file)
  #plot on Google Maps
  alpha<-0.50  #transparency
  #bb <- qbbox(lat=lats, lon=lons)
  #bb <- qbbox(lat=lats, lon=lons, TYPE="quantile")
  bb <- qbbox(lat=YLIMS, lon=XLIMS, TYPE="quantile")
  #par(pty="s");MyMap <- GetMap.bbox(bb$lonR, bb$latR)  #Openstreet map server (doesn't seem to work)
  par(pty="s");MyMap <- GetMap.bbox(bb$lonR, bb$latR,urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "./mapTiles/Google/")
  PlotOnStaticMap(MyMap,lat=LAT,lon=LON,col="black",pch=16,FUN=points,TrueProj = TRUE,mar=c(2,2,3,2))
  if(plotstidTF)PlotOnStaticMap(MyMap,lat=LAT,lon=LON,col="black",FUN=text,TrueProj = TRUE,add=TRUE,labels=rownames(station.info.sub))
  title(main=xmain,cex.main=1.5)
  mtext(xsub,side=1,cex=1.1)


  image.coords <- LatLon2XY.centered(MyMap, lat=as.vector(lats.mat), lon=as.vector(lons.mat))
  xnew.mat<-matrix(image.coords$newX,ncol=ncol(lons.mat),nrow=nrow(lons.mat))
  xnew<-xnew.mat[,1]
  ynew.mat<-matrix(image.coords$newY,ncol=ncol(lats.mat),nrow=nrow(lats.mat))
  ynew<-ynew.mat[1,]

  #figure out color scheme & zlims
  COLS<-c("white","violet","lightblue","blue","yellow","orange","red") #JCL:  just a subset of colors--not to conflict with trajectory color
  ZLIMS<-range(zmat,na.rm=T)
  colsc<-designer.colors(n=64,alpha=alpha,col=COLS)
  colsc.legend<-designer.colors(n=64,alpha=1.0,col=COLS)
  if(var=="U.cor"|var=="V.cor"|var=="T.cor"|var=="WSPD.cor"){
    require(viridis)
    ZLIMS<-c(0,0.8)
    #COLS<-c("white","yellow","orange","red") #JCL:  just a subset of colors--not to conflict with trajectory color
    #colsc<-designer.colors(n=64,alpha=alpha,col=COLS)
    #colsc.legend<-designer.colors(n=64,alpha=1.0,col=COLS)
    #colsc<-rev(heat.colors(n=64,alpha=alpha))
    #colsc.legend<-rev(heat.colors(n=64,alpha=1.0))
    colsc<-plasma(n=64,alpha=alpha)
    colsc.legend<-plasma(n=64,alpha=1.0)
  } #if(var=="U.cor"|var=="V.cor"){
  if(var=="U.bias"|var=="V.bias"|var=="T.bias"|var=="WSPD.bias"){
    ZLIMS<-c(-1,+1)
    if(var=="T.bias")ZLIMS<-c(-3,+3)
    colsc<-two.colors(n=64,alpha=alpha,start="darkblue",end="red",middle="white")
    colsc.legend<-two.colors(n=64,alpha=1.0,start="darkblue",end="red",middle="white")
  } #if(var=="U.bias"|var=="V.bias"){
  if(var=="U.rmse"|var=="V.rmse"|var=="T.rmse"|var=="WSPD.rmse"){
    ZLIMS<-c(1,3.0)
    colsc<-tim.colors(n=60,alpha=alpha)
    colsc.legend<-tim.colors(n=60,alpha=1.0)
    #require(viridis)
    #colsc<-viridis(n=64,alpha=alpha)
    #colsc.legend<-viridis(n=64,alpha=1.0)
  } #if(var=="U.rmse"|var=="V.rmse"){
  zmat2<-zmat
  zmat2[zmat2<ZLIMS[1]]<-ZLIMS[1];zmat2[zmat2>ZLIMS[2]]<-ZLIMS[2]   #make sure everything gets plotted on colorscale
  image(x=xnew,y=ynew,z=zmat2,add=TRUE,col=colsc,zlim=ZLIMS)
  image.plot(x=xnew,y=ynew,z=zmat2,legend.only=TRUE,add=TRUE,col=colsc.legend,legend.lab=var,zlim=ZLIMS)
  PlotOnStaticMap(MyMap,lat=LAT,lon=LON,col="black",pch=16,FUN=points,TrueProj = TRUE,add=T)
 
  figfilenm<-paste0(mettype,"_Errstats_Map_",start.file,"to",end.file,"_",var,".png")
  dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"generated"))

} #if(!plotgooglemapTF){

} #for(i in 1:length(vars)){


