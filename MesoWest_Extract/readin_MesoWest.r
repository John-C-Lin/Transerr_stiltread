#Reads in MesoWest data downloaded from API--e.g., 
#First written on June 20th, 2017 by John C. Lin (John.Lin@utah.edu)

###############
token <- 'demotoken'
# JCL(210321): for Uintah Basin analyses, choose whole years between 2016 and 2020 for analyses and save objects on annual timsecale to facilitate analyses later
starts<-c("201501010000","201601010000","201701010000","201801010000","201901010000","202001010000","202101010000","202201010000","202301010000")[8:9]
ends  <-c("201512312300","201612312300","201712312300","201812310000","201912312300","202012312300","202112312300","202212312300","202312312300")[8:9]

stids <- NULL
#Key MesoWest sites in SLV recommended by Alex Jacques
stids <- c("FPN","FPS","HERUT","MTMET","NAA","NHMU","SUNUT","TRJO","UFD09","WBB","FARM","TPC","K36U","KSLC",
           "KTVY","KU42","QBV","QED","QH3","QHW","QLN","QMG","QNP","QRP","QSA","BAC","CEN","KIJ","UT11","UT12",
           "UT20","UT201","UT23","UT248","UT3","UT5","UT7","UT9","UTALP","UTBIG","UTCDF","UTCHL","UTCOL","UTDAN",
           "UTDCD","UTDWY","UTHEB","UTJUN","UTLGP","UTLPC","UTMFS","UTORM","UTPCR","UTPKL","UTPLC","UTPR4","UTQRY",
           "UTSTR","UTSVC","UTTPD","UTWAN","UTWLC","UCC14")[-c(1:4)]
#stids<-stids[!stids%in%c("KSLC","KU42","QHW")]
#Key MesoWest sites in the Uintah Basin, as recommended by Chris Foster (chris.foster9195@gmail.com)
stids <- c(stids,c("QFL","QRS","UBCSP","UBHSP","KVEL","UBRDW"))
stids <- c(stids,c("A1633")) # additional RedWash site (to be used as substitute for UBHSP site in 2015)
#More MesoWest sites in the Uintah Basin to flesh out those mentioned above
stids <- c(stids,c("BRAU1","HWAU1","CCRU1","KPRU1","NWRU1","CHPU1","DIAU1","YLSU1","KHCR","BLAU1","KVEL","K74U",
                 "UINU1","SFLU1","RVZU1","HSRU1","FIVU1","USWU1","MCKU1"<"BRPU1","MCKU1","WNTU1","UPRU1","BCRU1",
                 "TS654","K4VO","DRAC2","HCKC2","DMMC2","E7309","E3808","E7258"))
#Additional MesoWest sites in northern SLC
stids <- c(stids,c("USDR2"))

avgtohrlyTF <- TRUE
###############
if(length(starts)!=length(ends)){stop("starts and ends have to have the same length")}

for(tt in 1:length(starts)){

  start<-starts[tt];end<-ends[tt]


for(i in 1:length(stids)){
  stid<-stids[i]
  # problem with UFD09 in 2018, it appears
  if(substring(start,1,4)=="2018"&stid=="UFD09"){print("Skipping UFD09 in 2018");next}
  if(substring(start,1,4)=="2018"&stid=="WBB"){print("Skipping WBB in 2018");next}
  if(substring(start,1,4)=="2019"&stid=="WBB"){print("Skipping WBB in 2019");next}
  if(substring(start,1,4)=="2020"&stid=="WBB"){print("Skipping WBB in 2020");next}

  resultname<-paste0(stid,"_",start,"to",end)
  print(paste(".......... Parsing: Station",stid,"from",start,"to",end,"..........."))

  strings <- try(readLines(paste0('http://api.mesowest.net/v2/stations/timeseries?',
                              '&token=', token, '&start=', start, '&end=', end,
                              '&stid=', stid, '&obtimezone=utc', '&output=csv')),silent=TRUE)
  if(length(strings)<10){print("data too limited; skip");next}
  lls<-strings[1:6]
  #read in LAT & LON of MesoWest site
  lats1<-as.numeric(strsplit(lls,split=" ")[[grep("LAT",lls)]])
  LAT<-lats1[!is.na(lats1)]
  lons1<-as.numeric(strsplit(lls,split=" ")[[grep("LON",lls)]])
  LON<-lons1[!is.na(lons1)]
  elev1<-as.numeric(strsplit(lls,split=" ")[[grep("ELEV",lls)]])
  ELEVATION.ft<-elev1[!is.na(elev1)]
  STATION<-paste(lls[grep("STATION",lls)],collapse=";")
  print(paste(STATION,LAT,LON,ELEVATION.ft,sep="  "))


  #Read in header comments and units lines
  colnms<-strsplit(strings[7],split=",")[[1]]
  colnms[colnms=="Date_Time"]<-"Time"
  units<-strsplit(strings[8],split=",")[[1]]

  strings <- strings[9:length(strings)]

if(length(strings)==1){
  dat <- read.table(text = strings, sep = ',', header = F, stringsAsFactors = F,col.names=colnms)
}else{
  if(sum(substring(colnms,1,17)%in%"weather_condition")>0){
    print("weather_condition found!")
    #deal with weather conditions that also have "comma" in the data--e.g., "Snow, Blowing Snow, Ice Fog"

    #(240526): search for special character sequence "\*" that bracket the weather condition data column
    #x <- strings[15639]
    parseweathercol <- function(x){
      ends <- gregexpr('\\"',x)[[1]]
      x1 <- substring(x,1,ends[1]-1)         # beginning of string
      x2 <- NULL;w0 <- 1
      for(ww in 1:(length(ends)/2)){
        xw <- gsub(",","-",xw)  # remove any extra commas within weather condition string
        xw <- gsub('\\"','',xw) # strip special character sequence '\"'
	if(ww==1)x2 <- xw
	if(ww>1) x2 <- paste0(x2,",",xw)
	w0 <- w0 + 2
      } # for(ww in 1:length(ends)/2){
      x3 <- substring(x,max(ends)+1,nchar(x))  # end of string
      result <- paste0(x1,x2,x3)
      return(result)
    } # parseweathercol <- function(x){
    strings <- sapply(strings,parseweathercol)
   } # if(sum(substring(colnms,1,17)%in%"weather_condition")>0){

  strings.split <-strsplit(strings,split=",")
  nn <- unlist(lapply(strings.split,length))
  nlast<-length(colnms)
    f<-function(x,nlast){
      if(length(x)==nlast){return(x)}
      if(length(x)<nlast){
        lastcol<-"NA"
      } #if(length(x)<nlast){
      y<-c(x,lastcol);return(y)
    } #f<-function(x){

  tmp<-unlist(lapply(strings.split,f,nlast=nlast))
  data<-matrix(tmp,byrow=T,ncol=length(colnms))
  dimnames(data)<-list(NULL,colnms)
  dat<-as.data.frame(data)
  dat$Station_ID <- stid
  dum<-dat

  #convert the factors into numeric values
  for(j in 1:ncol(dat)){
    if(is.factor(dat[,j])){
      tmp<-as.numeric(levels(dat[,j])[as.numeric(dat[,j])])
      if(sumNA(tmp)==length(tmp))tmp<-levels(dat[,j])[as.numeric(dat[,j])]
      dat[,j]<-tmp
    } #if(is.factor(dat[,j])){
  } #for(j in 1:ncol(dat)){

} #if(length(unique(nn))==1){

  dat$Time <- as.POSIXct(dat$Time, tz = 'UTC', format = '%Y-%m-%dT%H:%M:%SZ')

  #convert wind vector to U- and V-components
  if("wind_direction_set_1"%in%colnames(dat) & "wind_speed_set_1"%in%colnames(dat)){
    print(paste("winddir & windspeed both available at:",STATION))
    winddir<-as.numeric(dat[,"wind_direction_set_1"]) #[degrees]
    windspd<-as.numeric(dat[,"wind_speed_set_1"])     #[m/s]
    print(paste(range(winddir,na.rm=TRUE),collapse=" "))
    V<-windspd*cos(pi*winddir/180)  #[m/s]
    U<-windspd*sin(pi*winddir/180)  #[m/s]
    Vwind<--1*V;Uwind<--1*U   #vector is direction wind is coming FROM, so need to switch signs
    if(FALSE){ dev.new();plot(sqrt(Uwind^2+Vwind^2),windspd,pch=16) } #if(FALSE){
    dat<-data.frame(dat,Uwind,Vwind)
  } #if("wind_direction_set_1"%in%colnames(dat) & "wind_speed_set_1"%in%colnames(dat)){

  dat$air_temp_set_1 <- as.numeric(dat$air_temp_set_1)
  dat$wind_speed_set_1 <- as.numeric(dat$wind_speed_set_1)

  DAT<-list(STATION=STATION,LAT=LAT,LON=LON,ELEVATION.ft=ELEVATION.ft,dat=dat)
  saveRDS(DAT,file=paste0("out/",resultname,".RDS"))
  print(paste(paste0(resultname,".RDS"),"written out"))
  gc()

if(avgtohrlyTF){
  fdat <- DAT$dat
  YYYYMMDDHH <- format(fdat$Time,tz='UTC',format='%Y%m%d%H')
  Uave <- tapply(fdat[,"Uwind"],YYYYMMDDHH,mean,na.rm=TRUE)
  Vave <- tapply(fdat[,"Vwind"],YYYYMMDDHH,mean,na.rm=TRUE)
  WINDave <- tapply(as.numeric(fdat[,"wind_speed_set_1"]),YYYYMMDDHH,mean,na.rm=TRUE)
  Tave <- tapply(as.numeric(fdat[,"air_temp_set_1"]),YYYYMMDDHH,mean,na.rm=TRUE)
  Time.hrly <- strptime(names(Uave),tz='UTC',format='%Y%m%d%H')
  Station_ID <- rep(unique(fdat[,"Station_ID"]),length(Time.hrly))
  dat <- data.frame(Station_ID,Time=Time.hrly,Uwind=Uave,Vwind=Vave,Windspd=WINDave,Tair=Tave)
  if(FALSE){ X11();plot(sqrt(Uave^2+Vave^2),WINDave,pch=16) } #if(FALSE){
  DAT <- list(STATION=STATION,LAT=LAT,LON=LON,ELEVATION.ft=ELEVATION.ft,dat=dat)
  saveRDS(DAT,file=paste0("out/",resultname,"_hrly.RDS"))
  print(paste(paste0(resultname,"_hrly.RDS"),"written out"))
  gc() 
} #if(avgtohrlyTF){
} #for(i in 1:length(stids)){


} #for(tt in 1:length(starts)){


