#Compare Obs vs Sim met variables to help quantify transport errors
# V3(201228): add WSPD (windspeed) as a variable whose errors can be determined
# V4(210130): add 'Timelims', a subset of period between 'starts' and 'ends' to calculate transport errors during short time period
#March 15th, 2019 by John C. Lin (John.Lin@utah.edu)

###################
#starts<- c('201509010000','201512010000','201603010000','201606010000','201609010000','201809010000','201812010000')[1:3]
#ends  <- c('201511302300','201602282300','201605312300','201608312300','201611302300','201811302300','201902282300')[1:3]
#starts <- c("201612010000","201703010000","201706010000","201709010000","201712010000","201803010000","201806010000","201809010000","201812010000")[1:7]
#ends  <- c("201702282300","201705312300","201708312300","201711302300","201802282300","201805312300","201808312300","201811302300","201902282300")[1:7]
#starts <- c("201908010000"); ends <- c("201909302300")
#starts<-c("201908010000","201910010000","201912010000")
#ends  <-c("201909302300","201911302300","202002292300")
#starts<-c("201903010000","202003010000")
#ends  <-c("201907312300","202012312300")
# JCL(210321): for Uintah Basin analyses, choose whole years between 2016 and 2020 for analyses and save objects on annual timsecale to facilitate analyses later
starts<-c("201601010000","201701010000","201801010000","201901010000","202001010000","202101010000","202201010000","202301010000","202401010000")[9]
ends  <-c("201612312300","201712312300","201812310000","201912312300","202012312300","202112312300","202212312300","202312312300","202412312300")[9]

#specify a subset of period between 'starts' and 'ends' to calculate transport errors during short time period
Timelims <- NULL #NULL means whole time period from 'start' to 'end'
#Timelims<-c("201908081800","201908082100")   #Landfill case study for GSV paper [UTC]
#Timelims<-c("201908081400","201908081700")   #Gravel pit case study for GSV paper [UTC]
#Timelims<-c("201909091400","201909091700")   #Gravel pit case study for GSV paper [UTC]

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

outputdir <- "out/"   #output from 'XtractHRRR_MergeMesoWest.r' 

mettype <- "HRRR"
vars <- c("U","V","WSPD","T")[1:4]
plotTF <- FALSE
###################

if(length(starts)!=length(ends)){stop("starts and ends have to have the same length")}

for(tt in 1:length(starts)){

start<-starts[tt];end<-ends[tt]

#output from "XtractHRRR_MergeMesoWest.r"
objname<-paste0(outputdir,"/",mettype,"_obs_",start,"to",end,".RDS")
dat<-readRDS(objname)$dat
station.info<-readRDS(objname)$station.info

#look for missing stations and remove them
cnms<-colnames(dat)
stids.new<-NULL
for(k in 1:length(stids)){
  if(length(grep(paste0(".",stids[k]),cnms))==0){print(paste("missing station data:",stids[k],"; skip it"));next}
  stids.new<-c(stids.new,stids[k])
} #for(k in 1:length(stids)){
stids<-stids.new

#extract subset of time
if(is.null(Timelims))timelims<-c(start,end)  #didn't specify time limits; just use whole dataset
if(!is.null(Timelims))timelims<-Timelims
t1<-as.POSIXct(strptime(timelims[1],"%Y%m%d%H%M"),tz="UTC")
t2<-as.POSIXct(strptime(timelims[2],"%Y%m%d%H%M"),tz="UTC")
sel<-dat[,"Time"]>=t1&dat[,"Time"]<=t2
if(sum(sel)==0){print(paste("No data in range:",paste(timelims,collapse=" "),"within file:",objname));next}
dat<-dat[sel,]


errvars<-c("bias","cor","rmse")
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
  }else{
    sim<-dat[,paste0(var,"sim.",stid)]; obs<-dat[,paste0(var,"obs.",stid)]
  } #if(var=="WSPD"){

  xbias<-mean(sim-obs,na.rm=T)
  xcor<-cor(x=sim,y=obs,use="pairwise.complete.obs",method="pearson")
  xrmse<-sqrt(mean((sim-obs)^2,na.rm=T))
  result[i,paste0(var,".",errvars)]<-c(xbias,xcor,xrmse)
if(plotTF){
  xmain<-paste(mettype,"at",stid,"\nR=",signif(xcor,3),"; bias=",signif(xbias,3)," RMSE=",signif(xrmse,3))
  xsub<-paste(timelims[1],"to",timelims[2])
  dev.new();par(cex.axis=1.3,cex.main=1.1,cex.lab=1.3)
  plot(obs,sim,pch=16,main=xmain,sub=xsub,xlab=paste0(var,"obs"),ylab=paste0(var,"sim"))
  lines(-20:20,-20:20,lwd=2)  #1:1 line
  dev.off()
} #if(plotTF){

} #for(j in 1:length(vars)){

} #for(i in 1:length(stids)){

  finalresult<-list(station.info=station.info,dat=result)
  finalresultname<-paste0(outputdir,"/",mettype,"_Errstats_",timelims[1],"to",timelims[2],".RDS")
  saveRDS(finalresult,file=finalresultname);print(paste(finalresultname,"written out"))
  gc()
} #for(tt in 1:length(starts)){


