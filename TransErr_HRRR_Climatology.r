#Compare Obs vs Sim met variables to help quantify transport errors
#Calculate transport errors on a CLIMATOLOGICAL basis (e.g., seasonal, diurnal, ...etc.)
#March 20th, 2019 by John C. Lin (John.Lin@utah.edu)

###################
#start <- '201509010000' #YYYYMMDDHHmm
#end   <- '201510312300' #YYYYMMDDHHmm
#start <- '201511010000' #YYYYMMDDHHmm
#start <- '201512010000' #YYYYMMDDHHmm
#end <- '201602282300'   #YYYYMMDDHHmm
#start <- '201603010000' #YYYYMMDDHHmm
#end <- '201605312300'   #YYYYMMDDHHmm
#start <- '201606010000' #YYYYMMDDHHmm
#end <- '201608312300'   #YYYYMMDDHHmm
#start <- '201609010000' #YYYYMMDDHHmm
#end <- '201711302300'   #YYYYMMDDHHmm
#end <- '201802282300'   #YYYYMMDDHHmm
#start <- '201908010000' #YYYYMMDDHHmm
#end <- '202009302300'   #YYYYMMDDHHmm
#end <- '202002292300'   #YYYYMMDDHHmm
start <- '202401010000' #YYYYMMDDHHmm
end   <- '202412312300' #YYYYMMDDHHmm

#central file with ALL possible MesoWest sites; loads object "stids"
source("stids_all.r")
stids.ALL<-stids

#select SUBSET of stations
#stids.SEL<-c("MTMET","KSLC","NAA")
#  stations away from mountains of Salt Lake Valley
#stids.SEL<-c("KSLC","UT20","WBB","QHW","UT23","UT12","UCC14","NAA","KU42")
#  stations in northern SLC
stids.SEL<-c("KSLC","NAA","UT20","USDR2","WBB")[1:4]
#stids.SEL<-stids.ALL
#Key MesoWest sites in SLV recommended by Alex Jacques
#stids<-c("FPN","FPS","HERUT","MTMET","NAA","NHMU","SUNUT","TRJO","UFD09","WBB","FARM","TPC","K36U","KSLC",
#         "KTVY","KU42","QBV","QED","QH3","QHW","QLN","QMG","QNP","QRP","QSA","BAC","CEN","KIJ","UT11","UT12",
#         "UT20","UT201","UT23","UT248","UT3","UT5","UT7","UT9","UTALP","UTBIG","UTCDF","UTCHL","UTCOL","UTDAN",
#         "UTDCD","UTDWY","UTHEB","UTJUN","UTLGP","UTLPC","UTMFS","UTORM","UTPCR","UTPKL","UTPLC","UTPR4","UTQRY",
#         "UTSTR","UTSVC","UTTPD","UTWAN","UTWLC","UCC14")
#Key MesoWest sites in the Uintah Basin, as recommended by Chris Foster (chris.foster9195@gmail.com)
#stids<-c(stids,c("QFL","QRS","UBCSP","UBHSP","KVEL","UBRDW"))
#More MesoWest sites in the Uintah Basin to flesh out those mentioned above
#stids<-c(stids,c("BRAU1","HWAU1","CCRU1","KPRU1","NWRU1","CHPU1","DIAU1","YLSU1","KHCR","BLAU1","K74U",
#                 "UINU1","SFLU1","RVZU1","HSRU1","FIVU1","USWU1","MCKU1"<"BRPU1","MCKU1","WNTU1","UPRU1","BCRU1",
#                 "TS654","K4VO","DRAC2","HCKC2","DMMC2","E7309","E3808","E7258"))
#stids<-stids[!stids%in%c("SFLU1")]  #problematic stations when extracting...
#Additional MesoWest sites in northern SLC
#stids<-c(stids,c("USDR2"))  #UofU MiniSodar2




UThrs<-formatC(seq(0,21,3),width=2,flag="0")   #hours selected for comparison [UTC]
mettype<-"HRRR"
vars<-c("U","V","T")[-3]
plotTF<-TRUE

#XLIMS<-NULL;YLIMS<-NULL  #set lat/lon limits for interpolation;  set to NULL if dynamically determine
#a)  Salt Lake Valley domain
XLIMS<-c(-112.1,-111.7);YLIMS<-c(40.4,41.1)  #set lat/lon limits for limits of map;  set to NULL if dynamically determine
#b)  Uintah Basin domain
#XLIMS<-c(-111.05,-108.9);YLIMS<-c(39.4,41.0)  #set lat/lon limits for limits of map;  set to NULL if dynamically determine
###################

#create time vector from start to end
tmp<-seq(from = strptime(start, format="%Y%m%d%H",tz = 'UTC'),
         to = strptime(end, format="%Y%m%d%H",tz = 'UTC'), by   = 'hour')
YYYYMMDDHHs.all<-format(tmp,format="%Y%m%d%H")
SEL.UThrs<-substring(YYYYMMDDHHs.all,9,10)%in%UThrs
YYYYMMDDHHs<-YYYYMMDDHHs.all[SEL.UThrs]

#loop through multiple outputs from "XtractHRRR_MergeMesoWestV1.r", and then create large object
YYYYs<-unique(substring(YYYYMMDDHHs,1,4))
xfiles<-list.files(pattern=paste0(mettype,"_obs_"))
xfiles<-xfiles[grep(".RDS",xfiles)]
xrg1<-regexpr("_obs_",xfiles)
xfiles<-xfiles[substring(xfiles,xrg1+5,xrg1+8)%in%YYYYs]
xfiles<-xfiles[xfiles!="HRRR_obs_201509010000to201510312300.RDS"]
vars.all1<-rep(vars,each=length(stids.ALL))
vars.all1<-paste0(vars.all1,"sim.",rep(stids.ALL,rep=length(vars)))
vars.all2<-rep(vars,each=length(stids.ALL))
vars.all2<-paste0(vars.all2,"obs.",rep(stids.ALL,rep=length(vars)))
colnms.all<-c(vars.all1,vars.all2)
dat.all<-matrix(NA,nrow=length(YYYYMMDDHHs),ncol=length(colnms.all))
dimnames(dat.all)<-list(YYYYMMDDHHs,colnms.all)
dat.all<-data.frame(dat.all)
for(i in 1:length(xfiles)){
  #objname<-paste0(mettype,"_obs_",start,"to",end,".RDS") #output from "XtractHRRR_MergeMesoWestV1.r"
  objname<-xfiles[i]
  tmp<-readRDS(objname)$dat
  print(paste("Reading in:",objname," ncol=",ncol(tmp)))
  Time<-format(tmp[,"Time"],format="%Y%m%d%H")
  dat.all[Time,colnames(tmp)[-1]]<-tmp[,-1]
  #dat.all<-rbind(dat.all,tmp)
  station.info<-readRDS(objname)$station.info
} #for(i in 1:length(xfiles)){

#extract stations selected, and plot map of where the stations are found
require(RgoogleMaps)
sites.info<-station.info[stids.SEL,]
LAT<-sites.info[,"LAT"]; LON<-sites.info[,"LON"]
bb <- qbbox(lat=YLIMS, lon=XLIMS, TYPE="quantile")
#MyMap <- GetMap.bbox(bb$lonR, bb$latR, maptype="terrain", frame=TRUE,API_console_key="AIzaSyAGcf5a3irbJOHT4CHcN8sQi5L77JOwgfI")
par(pty="s");MyMap <- GetMap.bbox(bb$lonR, bb$latR,urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "./mapTiles/Google/")
PlotOnStaticMap(MyMap,lat=LAT,lon=LON,col="black",pch=16,FUN=points,TrueProj = TRUE,mar=c(2,2,3,2))
PlotOnStaticMap(MyMap,lat=LAT+0.01,lon=LON,col="blue",FUN=text,TrueProj = TRUE,add=TRUE,labels=rownames(sites.info))
figfilenm<-"sitemap.png"
dev.copy(png,filename=figfilenm);print(paste(figfilenm,"generated"))


#look for missing stations and remove them
cnms<-colnames(dat.all)
f<-function(x)return(all(is.na(x)))
sel<-apply(dat.all,2,f)
if(sum(sel)>0)print(paste("removing following columns because all NAs:    ",paste(cnms[sel],collapse=" ")))
dat.all<-dat.all[,!sel]

#extract SUBSET of specified stations 
stids2<-unique(substring(colnames(dat.all),6,nchar(colnames(dat.all))))
SEL<-stids.SEL%in%stids2
stids.SEL<-stids.SEL[SEL]
if(!all(SEL)){print(paste("MISSING stations from analyses:",paste(stids.SEL[!SEL])));stop}
stids<-stids.SEL
stids3<-substring(colnames(dat.all),6,nchar(colnames(dat.all)))
DAT<-dat.all[,stids3%in%stids.SEL]

#carry out analyses PER 3-month season
YYYYMMDDHHs<-rownames(DAT)
MMs<-substring(YYYYMMDDHHs,5,6)
#MMsels<-list(c("12","01","02"),c("03","04","05"),c("06","07","08"),c("09","10","11"))[1]
MMsels<-list(c("12","01","02"),c("08","09"),c("10","11"))

for(tt in 1:length(MMsels)){
  #MMsel<-c("12","01","02")  #winter time
  MMsel<-MMsels[[tt]]
  sel<-MMs%in%MMsel   
  dat<-DAT[sel,]
#collapse ALL sites (currently arranged in multiple columns) into one column
for(j in 1:length(vars)){
  print(paste("var =",var))
  var<-vars[j]
  #  determine subset of sites that have both simulations and observations
  simcolnms<-colnames(dat)[grep(paste0(var,"sim."),colnames(dat))]
  obscolnms<-colnames(dat)[grep(paste0(var,"obs."),colnames(dat))]
  stids.sim<-substring(simcolnms,6,nchar(simcolnms))
  stids.obs<-substring(simcolnms,6,nchar(obscolnms))
  stids.SUB<-intersect(stids.sim,stids.obs)
  sim<-dat[,paste0(var,"sim.",stids.SUB)]
  obs<-dat[,paste0(var,"obs.",stids.SUB)]
  YYYYMMDDHH<-rep(rownames(dat),ncol(sim))
  sim<-as.vector(as.matrix(sim))
  obs<-as.vector(as.matrix(obs))
  xbias<-mean(sim-obs,na.rm=T)
  xcor<-cor(x=sim,y=obs,use="pairwise.complete.obs",method="pearson")
  xrmse<-sqrt(mean((sim-obs)^2,na.rm=T))
  HH<-substring(YYYYMMDDHH,9,10)
  sim.ave<-tapply(sim,HH,mean,na.rm=T)
  obs.ave<-tapply(obs,HH,mean,na.rm=T)
  xbias.ave<-tapply(sim-obs,HH,mean,na.rm=T)
  f<-function(x)return(sqrt(mean(x^2,na.rm=T)))
  xrmse.ave<-tapply(sim-obs,HH,f)
  Time<-strptime(YYYYMMDDHH,format="%Y%m%d%H",tz="UTC")
if(plotTF){
  #library(MASS)
  #dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3)
  #plot(Time,obs,pch=16,col="black")
  #points(Time,sim,pch=16,col="blue")

  #overall scatterplot
  ylims<-c(-8,8);xlims<-ylims
  xmain<-paste(mettype,"at:",paste(stids.SUB.collapse=" "),"\nMons:",paste(MMsel,collapse=" "),"\nR=",signif(xcor,3),
                       " bias=",signif(xbias,3)," RMSE=",signif(xrmse,3))
  xsub<-paste(unique(format(Time,format="%Y%m")),collapse=" ")
  dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3,cex.sub=0.7)
  plot(obs,sim,pch=16,main=xmain,sub=xsub,xlab=paste0(var,"obs"),ylab=paste0(var,"sim"),ylim=ylims,xlim=xlims)
  lines(-40:40,-40:40,lwd=2)  #1:1 line
  figfilenm<-paste0(mettype,"_",var,"_scatter_",paste(MMsel,collapse="_"),".png")
  dev.copy(png,filename=figfilenm);print(paste(figfilenm,"generated"))

  #diurnal plots
  ylims<-range(c(sim.ave,obs.ave),na.rm=T)
  if(var%in%c("U","V"))ylims<-c(-1,2)
  xmain<-paste(mettype,"at:",paste(stids.SUB,collapse=" "),"\nMons:",paste(MMsel,collapse=" "))
  xsub<-paste(unique(format(Time,format="%Y%m")),collapse=" ")
  dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3,cex.sub=0.7)
  plot(as.numeric(names(obs.ave)),obs.ave,pch=16,type="o",col="black",main=xmain,sub=xsub,xlab="Hour of Day [UTC]",ylab=var,ylim=ylims,lwd=2)
  lines(as.numeric(names(sim.ave)),sim.ave,pch=16,type="o",col="blue",lwd=2)
  abline(h=0,lty=2)
  legend("topleft",legend=c("Obs","Sim"),col=c("black","blue"),text.col=c("black","blue"),pch=16) 
  figfilenm<-paste0(mettype,"_",var,"_simvsobs_diurnal_",paste(MMsel,collapse="_"),".png")
  dev.copy(png,filename=figfilenm);print(paste(figfilenm,"generated"))

  #diurnal BIAS + RMSE plots
  #ylims<-range(c(xbias.ave,xrmse.ave),na.rm=T)
  ylims<-c(-1,3.0)
  xmain<-paste(mettype,"at:",paste(stids.SUB,collapse=" "),"\nMons:",paste(MMsel,collapse=" "))
  xsub<-paste(unique(format(Time,format="%Y%m")),collapse=" ")
  dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3,cex.sub=0.7)
  plot(as.numeric(names(xbias.ave)),xbias.ave,pch=16,type="o",col="black",main=xmain,sub=xsub,xlab="Hour of Day [UTC]",ylab=paste(var,"error"),ylim=ylims,lwd=2)
  abline(h=0,lty=2)
  lines(as.numeric(names(xrmse.ave)),xrmse.ave,pch=16,type="o",col="orange",lwd=2)
  legend("topleft",legend=c("Bias","RMSE"),col=c("black","orange"),text.col=c("black","orange"),pch=16) 
  figfilenm<-paste0(mettype,"_",var,"_bias_rmse_diurnal_",paste(MMsel,collapse="_"),".png")
  dev.copy(png,filename=figfilenm);print(paste(figfilenm,"generated"))
  
} #if(plotTF){
} #for(j in 1:length(vars)){

} #for(tt in 1:length(MMsels)){

