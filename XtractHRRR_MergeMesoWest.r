# Calls Ben Fasoli's implementation of R code to directly read in variables stored in ARL-formatted met files: https://github.com/benfasoli/stiltread/blob/master/README.md
# Limitations
# Returned raster layers are limited to files that use Lambert Conformal Conic for the map projection, which is typical for midlatitudes.
# Upper levels of pressure variables are often recorded as a difference from the base level. This calculation needs to be performed by the user.
# V2(190320): carry out bilinear interpolation of met data to location of MesoWest site instead of nearest gridcell
# V3(201112): deal with new HRRR filename format: e.g., 20190710_00-05_hrrr, 20190710_06-11_hrrr, 20190710_12-17_hrrr, 20190710_18-23_hrrr   
# March 11th, 2019 by John C. Lin (John.Lin@utah.edu)

library(raster)
#devtools::install_github('benfasoli/stiltread')
library(stiltread)  #;install_stiltread()

#####################
#start <- '201509010000'; end   <- '201510312300' #YYYYMMDDHHmm
#start <- '201512010000'; end <- '201602282300'   #YYYYMMDDHHmm
#start <- '201603010000'; end <- '201605312300'   #YYYYMMDDHHmm
#start <- '201606010000'; end <- '201608312300'   #YYYYMMDDHHmm
#start <- '201609010000'; end <- '201611302300'   #YYYYMMDDHHmm

#starts<-c('201509010000','201512010000','201603010000','201606010000','201609010000','201809010000','201812010000')[1:3]
#ends  <-c('201511302300','201602282300','201605312300','201608312300','201611302300','201811302300','201902282300')[1:3]
#starts<-c("201612010000","201703010000","201706010000","201709010000","201712010000","201803010000","201806010000","201809010000","201812010000")[1:7]
#ends  <-c("201702282300","201705312300","201708312300","201711302300","201802282300","201805312300","201808312300","201811302300","201902282300")[1:7]
#starts<-c("201908010000"); ends<-  c("201909302300")
#starts<-c("201908010000","201910010000","201912010000")
#ends<-  c("201909302300","201911302300","202002292300")
#starts<-c("201903010000","202003010000"); ends  <-c("201907312300","202012312300")

# JCL(210321): for Uintah Basin analyses, choose whole years between 2016 and 2020 for analyses and save objects on annual timsecale to facilitate analyses later
starts<-c("201501010000","201601010000","201701010000","201801010000","201901010000","202001010000","202101010000","202201010000","202301010000","202401010000")[10]
ends  <-c("201512312300","201612312300","201712312300","201812312300","201912312300","202012312300","202112312300","202212312300","202312312300","202412312300")[10]


#stids<-c("KSLC","KU42","QHW")
#Key MesoWest sites in SLV recommended by Alex Jacques
stids<-c("FPN","FPS","HERUT","MTMET","NAA","NHMU","SUNUT","TRJO","UFD09","WBB","FARM","TPC","K36U","KSLC",
         "KTVY","KU42","QBV","QED","QH3","QHW","QLN","QMG","QNP","QRP","QSA","BAC","CEN","KIJ","UT11","UT12",
         "UT20","UT201","UT23","UT248","UT3","UT5","UT7","UT9","UTALP","UTBIG","UTCDF","UTCHL","UTCOL","UTDAN",
         "UTDCD","UTDWY","UTHEB","UTJUN","UTLGP","UTLPC","UTMFS","UTORM","UTPCR","UTPKL","UTPLC","UTPR4","UTQRY",
         "UTSTR","UTSVC","UTTPD","UTWAN","UTWLC","UCC14")
#Key MesoWest sites in the Uintah Basin, as recommended by Chris Foster (chris.foster9195@gmail.com)
stids<-c(stids,c("QFL","QRS","UBCSP","UBHSP","KVEL","UBRDW"))
stids<-c(stids,c("A1633")) # addditional RedWash site (to be used as substitute for UBHSP in 2015)
#More MesoWest sites in the Uintah Basin to flesh out those mentioned above
stids<-c(stids,c("BRAU1","HWAU1","CCRU1","KPRU1","NWRU1","CHPU1","DIAU1","YLSU1","KHCR","BLAU1","K74U",
                 "UINU1","SFLU1","RVZU1","HSRU1","FIVU1","USWU1","MCKU1"<"BRPU1","MCKU1","WNTU1","UPRU1","BCRU1",
                 "TS654","K4VO","DRAC2","HCKC2","DMMC2","E7309","E3808","E7258"))
stids<-stids[!stids%in%c("SFLU1")]  #problematic stations when extracting...
#Additional MesoWest sites in northern SLC
stids<-c(stids,c("USDR2"))  #UofU MiniSodar2

Mdir<-"MesoWest_Extract/out/"
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group8/hrrr/data/wasatch/"
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group8/hrrr/data/utah/"
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group12/hrrr/hrrr/"
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group11/hrrr-gsv-slv/"   #HRRR chopped over SLV
#metdir<-"/uufs/chpc.utah.edu/common/home/lin-group7/jcl/CH4_inversion/CH4_inversion_Uintah/CH4_inversion_Uintah_HYSPLIT-STILT/out/HPL/met/"   #HRRR chopped over Uintah Basin
metdir<-"/uufs/chpc.utah.edu/common/home/lin-group20/jcl/CH4_inversion/CH4_inversion_Uintah/CH4_inversion_Uintah_HYSPLIT-STILT_HRRR/out/HPL/met/"   #HRRR chopped over Uintah Basin
mettype<-"HRRR"
#UThrs <- formatC(seq(0,21,3),width=2,flag="0")   #hours selected for comparison [UTC]
UThrs <- formatC(seq(0,23,1),width=2,flag="0")   #hours selected for comparison [UTC]
#UThrs <- formatC(seq(20,23,1),width=2,flag="0")   #hours selected for comparison [UTC]
XtractTF <- TRUE              #extract met data?
overwriteTF <- FALSE   #whether or not to overwrite previous results
bilinear.interpTF <- TRUE      #whether to carry out bilinear interpolation
xres <- 0.004; yres <- 0.004 #interpolation grid spacing [deg lat/lon]
#####################
if(length(starts)!=length(ends)){stop("starts and ends have to have the same length")}

for(tt in 1:length(starts)){

start<-starts[tt];end<-ends[tt]

#read in lat/lon coordinates of surface sites
coords<-NULL
stids.new<-NULL
longnames<-NULL
for(i in 1:length(stids)){
  stid<-stids[i]
  objname<-paste0(stid,"_",start,"to",end,"_hrly.RDS")
  if(!file.exists(paste0(Mdir,objname))){print(paste(objname,"missing"));next}
  stids.new<-c(stids.new,stid)
  print(paste("Reading in:",objname))
  DAT<-readRDS(paste0(Mdir,objname))
  coords<-rbind(coords,c(DAT$LON,DAT$LAT,DAT$ELEVATION.ft))
  longnames<-rbind(longnames,DAT$STATION)
  gc()
} #for(i in 1:length(stids)){
stids<-stids.new  #accounts for missing stations...
dimnames(coords)<-list(stids,c("LON","LAT","ELEVATION.ft"))
coords<-data.frame(coords,STATION_INFO=longnames)

tmp<-seq(from = strptime(start, format="%Y%m%d%H",tz = 'UTC'),
         to = strptime(end, format="%Y%m%d%H",tz = 'UTC'), by   = 'hour')
YYYYMMDDHHs.all<-format(tmp,format="%Y%m%d%H")
SEL.UThrs<-substring(YYYYMMDDHHs.all,9,10)%in%UThrs
YYYYMMDDHHs<-YYYYMMDDHHs.all[SEL.UThrs]

metresultname<-paste0("out/",mettype,"_",start,"to",end,".RDS")
if(XtractTF){ # -------------------------------------------------------------------------------------------------------------------------------------
  if(overwriteTF){
    Time <- strptime(YYYYMMDDHHs,tz="UTC",format="%Y%m%d%H")
    COLNMS <- paste0(rep(c("Usim.","Vsim.","Tsim."),each=length(stids)),rep(stids,3))
    result <- matrix(NA,nrow=length(YYYYMMDDHHs),ncol=length(COLNMS))
    dimnames(result) <- list(YYYYMMDDHHs,COLNMS)
    result <- data.frame(Time,result)
    i0 <- 1  #row from which start filling result
  } else {
    result <- readRDS(metresultname)
    COLNMS <- colnames(result)[-1]
    sumNA <- function(x)return(sum(is.na(x)))
    numNA <- apply(result,1,sumNA)
    i0 <- which(numNA==(ncol(result)-1))[1]
    print(paste("Starting from row #:",i0))
  } #if(overwriteTF){

for(i in i0:length(YYYYMMDDHHs)){
  YYYYMMDDHH<-YYYYMMDDHHs[i]
  print(paste("Processing....",YYYYMMDDHH))
  YYYYMMDD<-substring(YYYYMMDDHH,1,8)
  HH<-substring(YYYYMMDDHH,9,10)
  #HRRR files are ONLY available in 6 hrly chunks: e.g., 20190710_00-05_hrrr, 20190710_06-11_hrrr, 20190710_12-17_hrrr, 20190710_18-23_hrrr   
  #  ARL archive documentation: "Each 1 hour forecast is concatenated to create a 6 hour data file"
  # tmp<-switch(HH, "03" = "00", "09" = "06", "15" = "12", "21" = "18")
  # if(!is.null(tmp))HH<-tmp
  # metfile<-paste0(YYYYMMDD,".",HH,"z.hrrra")

  HH1<-formatC(floor(as.numeric(HH)/6)*6,width=2,flag="0")
  HH2<-formatC(as.numeric(HH1)+5,width=2,flag="0")
  metfile<-paste0(YYYYMMDD,"_",HH1,"-",HH2,"_hrrr")

  if(!file.exists(paste0(metdir,metfile))){print(paste(metfile,"does NOT exist;  skip!"));next}
  yy<-as.numeric(substring(YYYYMMDDHH,3,4))
  mm<-as.numeric(substring(YYYYMMDDHH,5,6))
  dd<-as.numeric(substring(YYYYMMDDHH,7,8))
  hh<-as.numeric(substring(YYYYMMDDHH,9,10))
  #call read_met_wind from stiltread R package
  nlvl<-0 #read surface winds ("U10M")
  #Since wind is grid relative, use read_met_wind instead of read_met to access wind fields. This is called in the same way as read_met, only no var argument is required. This returns a rasterStack with u and v layers that have been rotated into +proj=longlat.
  # 'try' deals with cases if read_met_wind crashes--e.g., wrong date specified or if variable does NOT exist !!!!#
  uv<- try(read_met_wind(paste0(metdir,metfile), yy = yy, mm = mm, dd = dd, hh = hh, lvl = nlvl),silent=TRUE)
  if(is.null(uv[1])){print(paste("uv is NULL!; Skip",metfile,"due to error in reading U/V: ",uv));next}
  if(class(uv)=="try-error"){print(paste("Skip",metfile,"due to error in reading U/V: ",uv));next}
  u2<-uv$u;v2<-uv$v
  gc()

  #returns a raster::RasterLayer containing data, grid coordinates, and projection information using the standard PROJ4 format.
  Tair <- read_met(paste0(metdir,metfile),var='T02M',
                        yy = yy, mm = mm, dd = dd, hh = hh, lvl = nlvl)   #2-m temperature [K]
  Tair <- raster::projectRaster(Tair, crs = "+proj=longlat")
  #plot(Tair); points(coords[,c("LON","LAT")],pch=17,col="black")

if(bilinear.interpTF){
  #!!!NOTE:  this is the major time sink for running this script!!!!#
  #carry out bilinear interpolation
  #xres <- 0.01; yres <- 0.01
  u2<- projectRaster(uv$u, res = c(xres, yres), crs = '+proj=longlat', method = 'bilinear')
  v2<- projectRaster(uv$v, res = c(xres, yres), crs = '+proj=longlat', method = 'bilinear')
  T2<- projectRaster(Tair, res = c(xres, yres), crs = '+proj=longlat', method = 'bilinear')
} #if(bilinear.interpTF){

  #X11();plot(uv$u,zlim=c(-10,10),main=paste("Uwind; lvl =",nlvl,"\n",metfile),xlim=c(-112.4,-111.5),ylim=c(40.4,41.1))
  #points(coords[,c("LON","LAT")],pch=17,col="black")
  #X11();plot(u2,zlim=c(-10,10),main=paste("Uwind; lvl =",nlvl,"\n",metfile),xlim=c(-112.4,-111.5),ylim=c(40.4,41.1))
  #points(coords[,c("LON","LAT")],pch=17,col="black")
  #X11();plot(Tair-273.15,main=paste("Tair; lvl =",nlvl,"\n",metfile),xlim=c(-112.4,-111.5),ylim=c(40.4,41.1))
  #points(coords[,c("LON","LAT")],pch=17,col="black")
  #X11();plot(T2-273.15,main=paste("Uwind; lvl =",nlvl,"\n",metfile),xlim=c(-112.4,-111.5),ylim=c(40.4,41.1))
  #points(coords[,c("LON","LAT")],pch=17,col="black")

  #extract U/V winds from HRRR at surface sites
  cn<-cellFromXY(u2,coords[,c("LON","LAT")])   #cell number coinciding with lon/lat coordinates
  if(sum(is.na(cn))>0)print(coords[is.na(cn),])
  #xyFromCell(u2,cellFromXY(u2,coords[,c("LON","LAT")]))   #'xyFromCell' is the opposite of 'cellFromXY'
  Usim<-u2[cn]; Vsim<-v2[cn]
  Tsim<-T2[cn] - 273.15   #[K]=>[C]
  result[i,COLNMS]<-c(Usim,Vsim,Tsim)
  saveRDS(result,file=metresultname);print(paste(metresultname,"written out"))
  gc()
} #for(i in 1:length(YYYYMMDDHHs)){
} #if(XtractTF){ -------------------------------------------------------------------------------------------------------------------------------------

#Merge model data with OBS (hourly averaged)
result<-readRDS(metresultname)
for(i in 1:length(stids)){
  stid<-stids[i]
  objname<-paste0(stid,"_",start,"to",end,"_hrly.RDS")
  obs<-readRDS(paste0(Mdir,objname))$dat
  sel<-obs[,"Time"]%in%result[,"Time"]

  result<-merge(result,obs[,c("Time","Uwind","Vwind","Tair")],by="Time",all.x=TRUE)
} #for(i in 1:length(stids)){
  colnms<-paste0(rep(c("Uobs.","Vobs.","Tobs."),length(stids)),rep(stids,each=3))
  col.names<-colnames(result)
  col.names[(ncol(result)-length(colnms)+1):ncol(result)]<-colnms
  colnames(result)<-col.names

  finalresult<-list(station.info=coords,dat=result)
  finalresultname<-paste0("out/",mettype,"_obs_",start,"to",end,".RDS")
  saveRDS(finalresult,file=finalresultname);print(paste(finalresultname,"written out"))

  gc()
} #for(tt in 1:length(starts)){

#From https://www.arl.noaa.gov/data/archives/hrrr/README.TXT:
#==============================================================
#==============================================================
#
#			HRRR 3 km Forecast Data Archive 
#                          Revised: 24 February 2016 
#
#________________________________________________________________________________
#
#Overview -
#
#NOAA ARL began an archive of the first hour of hourly HRRR 3 km meteorological data from NOAA NCEP in June 2015.  Each 1 hour forecast is concatenated to create a 6 hour data file beginning with 00z - 05z in the first file of the day. The first file of the day is called:
#
#hysplit.YYYYMMDD.00z.hrrra
#
#Subsequent files that day are named as follows:
#
#hysplit.YYYYMMDD.06z.hrrra - containing data from 06 - 11z
#hysplit.YYYYMMDD.12z.hrrra - containing data from 12 - 17z
#hysplit.YYYYMMDD.18z.hrrra - containing data from 18 - 23z
#
#NOTE:  The Total Kinetic Energy (TKEN) field is zero from the initial date of the
#archive through the 12 UTC August 23, 2016 data set.  The TKE field is
#available beginning at 18 UTC on August 23, 2016.
#________________________________________________________________________________
#
#Additional Data Set Details
#
#	3 km lambert conformal grid 
#	1799x1059 points 
#	Spatial domain: 12.14N 122.72W to 47.844N 60.9155W
#	36 Levels (hPa): 1.0 0.9990 0.9960 0.9905 0.9810 0.9670 0.9490 0.9275 
#        0.9040 0.8785 0.8500 0.8185 0.7840 0.7465 0.7060 0.6625 0.6160 0.5670 
#	0.5195 0.4767 0.4385 0.4041 0.3729 0.3449 0.3198 0.2767 0.2412 0.2118 
#	0.1836 0.1553 0.1271 0.0989 0.0718 0.0527 0.0304 0.0124 
#
#	Sfc Variables:         24 SHGT PRSS MSLP TPP1 DIFR T02M DP2M PBLH DSWF 
#				  ULWF U10M V10M VSBY CSNO CICE CFZR CRAI LIB4 
#				  CAPE CINH TCLD REFC CLDB CLDT 
#	Upper Levels:           8 PRES TEMP UWND VWND WWND DIFW SPHU TKEN
#
#	MODEL TYPE:         HRRR
#	VERT COORD:         1
#	POLE LAT:           38.5000
#	POLE LON:           262.5000
#	REF LAT:            38.5000
#	REF LON:            262.5000 
#	REF GRID:           3.0
#	ORIENTATION:        0.
#	CONE ANGLE:         38.5000
#	SYNC X:             1.
#	SYNC Y:             1.
#	SYNC LAT:           21.1400
#	SYNC LON:           237.2800
#	NUMB X:             1799
#	NUMB Y:             1059
#	NUMB LEVELS:        36
