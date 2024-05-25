# Compare Obs vs Sim met variables to help quantify transport errors and generate TIME SERIES 
# Based on "TransErr_HRRRV2.r"
# V2(210130): option to add high-freq met obs (sub-hourly)
# December 18th, 2020 by John C. Lin (John.Lin@utah.edu)

###################
start.file <- c("201908010000"); end.file <- c("201909302300")
#timelims<-c(start.file,end.file)   #time range to generate analyses
#timelims<-c("201908070700","201908100700")   #time range to generate analyses [UTC]
# GSV paper case studies
#1) One-day time series
#timelims<-c("201908080700","201908090700")   
#timelims<-c("201909090700","201909100700")   
#2) Zoom into a few hours
timelims<-c("201908081800","201908082100")   #Landfill case study for GSV paper [UTC]
#timelims<-c("201908081400","201908081700")   #Gravel pit case study for GSV paper [UTC]
#timelims<-c("201909091400","201909091700")   #Gravel pit case study for GSV paper [UTC]

#Key MesoWest sites in SLV recommended by Alex Jacques
stids<-c("FPN","FPS","HERUT","MTMET","NAA","NHMU","SUNUT","TRJO","UFD09","WBB","FARM","TPC","K36U","KSLC",
         "KTVY","KU42","QBV","QED","QH3","QHW","QLN","QMG","QNP","QRP","QSA","BAC","CEN","KIJ","UT11","UT12",
         "UT20","UT201","UT23","UT248","UT3","UT5","UT7","UT9","UTALP","UTBIG","UTCDF","UTCHL","UTCOL","UTDAN",
         "UTDCD","UTDWY","UTHEB","UTJUN","UTLGP","UTLPC","UTMFS","UTORM","UTPCR","UTPKL","UTPLC","UTPR4","UTQRY",
         "UTSTR","UTSVC","UTTPD","UTWAN","UTWLC","UCC14")
#Key MesoWest sites in the Uintah Basin, as recommended by Chris Foster (chris.foster9195@gmail.com)
stids<-c(stids,c("QFL","QRS","UBCSP","UBHSP","KVEL","UBRDW"))
#More MesoWest sites in the Uintah Basin to flesh out those mentioned above
stids<-c(stids,c("BRAU1","HWAU1","CCRU1","KPRU1","NWRU1","CHPU1","DIAU1","YLSU1","KHCR","BLAU1","K74U",
                 "UINU1","SFLU1","RVZU1","HSRU1","FIVU1","USWU1","MCKU1"<"BRPU1","MCKU1","WNTU1","UPRU1","BCRU1",
                 "TS654","K4VO","DRAC2","HCKC2","DMMC2","E7309","E3808","E7258"))
stids<-stids[!stids%in%c("SFLU1")]  #problematic stations when extracting...
#Additional MesoWest sites in northern SLC
stids<-c(stids,c("USDR2"))  #UofU MiniSodar2

Mdir<-"MesoWest_Extract/"
# stations in northern SLC
stidSELs <- c("KSLC","NAA","UT20","USDR2","QRP","WBB")[1:5]
mettype<-"HRRR"
vars<-c("U","V","WSPD","T")[1:3]
errvars<-c("bias","cor","rmse")
scatterplotTF<-FALSE
plot.hifreqobsTF<-TRUE  #plot high-freq met obs (sub-hourly)?
###################


#output from "XtractHRRR_MergeMesoWestV1.r"
objname<-paste0(mettype,"_obs_",start.file,"to",end.file,".RDS")
dat<-readRDS(objname)$dat
station.info<-readRDS(objname)$station.info

#subset dataset to specified time range 
t1<-as.POSIXct(strptime(timelims[1],"%Y%m%d%H%M"),tz="UTC")
t2<-as.POSIXct(strptime(timelims[2],"%Y%m%d%H%M"),tz="UTC")
sel<-dat[,"Time"]>=t1&dat[,"Time"]<=t2
dat<-dat[sel,]

#look for missing stations and remove them
cnms<-colnames(dat)
stids.new<-NULL
for(k in 1:length(stidSELs)){
  if(length(grep(paste0(".",stidSELs[k]),cnms))==0){print(paste("missing station data:",stidSELs[k],"; skip it"));next}
  stids.new<-c(stids.new,stidSELs[k])
} # for(k in 1:length(stidSELs)){
stids<-stids.new

colnms<-paste0(rep(vars,each=length(errvars)),".",rep(errvars,length(vars)))
#result<-data.frame(rep(NA,length(stids)*length(colnms)),row.names=stids,col.names=colnms)
result<-matrix(NA,nrow=length(stids),ncol=length(colnms))
dimnames(result)<-list(stids,colnms)
for(i in 1:length(stids)){
  stid<-stids[i]

for(j in 1:length(vars)){
  var<-vars[j]
  if(var=="WSPD"){
    sim<-sqrt(dat[,paste0("Usim.",stid)]^2+dat[,paste0("Vsim.",stid)]^2)
    obs<-sqrt(dat[,paste0("Uobs.",stid)]^2+dat[,paste0("Vobs.",stid)]^2)
    YLIMS<-c(0,6)
  }else{
    sim<-dat[,paste0(var,"sim.",stid)]; obs<-dat[,paste0(var,"obs.",stid)]
    #YLIMS<-c(-3,3)
    YLIMS<-c(-4,4)
  } #if(var=="Wspd"){

  # I. Generate summary scatter plots and statistics
  xbias<-mean(sim-obs,na.rm=T)
  xcor<-cor(x=sim,y=obs,use="pairwise.complete.obs",method="pearson")
  xrmse<-sqrt(mean((sim-obs)^2,na.rm=T))
  result[i,paste0(var,".",errvars)]<-c(xbias,xcor,xrmse)
  xmain<-paste(mettype,"at",stid,"\nR=",signif(xcor,3),"; bias=",signif(xbias,3)," RMSE=",signif(xrmse,3))
  xsub<-paste(timelims[1],"to",timelims[2],"[UTC]")
if(scatterplotTF){
  dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3)
  plot(obs,sim,pch=16,main=xmain,sub=xsub,xlab=paste0(var,"obs"),ylab=paste0(var,"sim"))
  lines(-20:20,-20:20,lwd=2)  #1:1 line
} #if(scatterplotTF){

  # II. Generate time series plots--use LOCAL TIME
  LT<-as.POSIXlt(dat[,"Time"],tz="MST")
  dev.new();par(cex.lab=1.3,cex.axis=1.1,cex.main=1.3)
  plot(LT,obs,pch=16,type="o",xlab="Time [MST]",ylab=var,main=xmain,sub=xsub,lwd=2,xaxt="n",ylim=YLIMS)
  tmp<-format(LT,"%m/%d/%Y")
  #  add tick marks every day
  at.tt.days<-unique(as.POSIXlt(tmp,tz="MST",format="%m/%d/%Y"))
  axis.POSIXct(1,at=at.tt.days,format="%m/%d/%y",tcl=0.4)
  #  add tick marks every 12 hours
  tt.min<-as.POSIXlt(min(unique(as.character(at.tt.days))),tz="MST")
  tt.max<-as.POSIXlt(max(unique(as.character(at.tt.days))),tz="MST")+24*3600
  axis.POSIXct(1,at=seq(tt.min,tt.max,12*3600),labels=FALSE,tcl=-0.4)
  #  add tick marks every 3 hours
  at.tt.3hrs<-seq(tt.min,tt.max,3*3600)
  sel<-as.numeric(at.tt.3hrs)%in%as.numeric(at.tt.days)  #don't plot label when date is already labelled
  axis.POSIXct(1,at=at.tt.3hrs[!sel],labels=TRUE,tcl=-0.4,format="%H:%M",cex.axis=1.1)
  #  add tick marks every hour
  at.tt.hrly<-seq(tt.min,tt.max,3600)
  if((as.numeric(t2)-as.numeric(t1))/3600 < 20){
    sel<-as.numeric(at.tt.hrly)%in%as.numeric(at.tt.3hrs)  #don't plot label when 3hrly is already labelled
    axis.POSIXct(1,at=at.tt.hrly[!sel],labels=TRUE,tcl=-0.2)   #add hrly labels if plot sub-daily timescales
  } else {
    axis.POSIXct(1,at=at.tt.hrly,labels=FALSE,tcl=-0.2)
  } # if((as.numeric(t2)-as.numeric(t1))/3600 < 20){
  lines(LT,sim,pch=17,type="o",lwd=2,col="blue")
  abline(h=0,lty=2)
  legend("topright",c("Obs","Sim"),pch=c(16,17),col=c("black","blue"),cex=1.3,text.col=c("black","blue"))
  #  plot high-freq met obs (sub-hourly)?
  if(plot.hifreqobsTF){
    obsname<-paste0(stid,"_",start.file,"to",end.file,".RDS")
    if(!file.exists(paste0(Mdir,obsname))){print(paste(obsname,"missing"));next}
    print(paste("read in",obsname))
    dat.hifreq<-readRDS(paste0(Mdir,obsname))$dat  #read in MesoWest obs (sub-hourly)
   if(var=="WSPD"){
     obs.hifreq<-sqrt(dat.hifreq[,"Uwind"]^2+dat.hifreq[,"Vwind"]^2)
   } else if(var=="T") {
     obs.hifreq<-dat.hifreq[,"air_temp_set_1"]
   } else {
     obs.hifreq<-dat.hifreq[,paste0(var,"wind")]
   } #if(var=="WSPD"){
   lines(dat.hifreq[,"Time"],obs.hifreq,type="l",pch=16,col="darkgray",lwd=1) 

  } # if(plot.hifreqobsTF){

  figfilenm<-paste0(mettype,"_tseries_",timelims[1],"to",timelims[2],"_",var,"_",stid,".png")
  dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"generated"))

} #for(j in 1:length(vars)){

} #for(i in 1:length(stids)){

