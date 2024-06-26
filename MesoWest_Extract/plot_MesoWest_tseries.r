# Plots time series of observations MesoWest stations generated by 'readin_MesoWest.r'
# February 3rd, 2021 by John C. Lin (John.Lin@utah.edu)


###################
start.file <- c("201908010000"); end.file <- c("201909302300")
timelims <- c(start.file,end.file)
# zoom into a few hours
#timelims<-c("201908081800","201908082100")   #Landfill case study for GSV paper [UTC]
#timelims<-c("201908081400","201908081700")   #Gravel pit case study for GSV paper [UTC]
timelims<-c("201909091400","201909091700")   #Gravel pit case study for GSV paper [UTC]

Mdir <- "/uufs/chpc.utah.edu/common/home/lin-group7/jcl/Transporterr_stiltread/MesoWest_Extract/"
# stations in northern SLC
stids <- c("KSLC","NAA","UT20","USDR2","QRP","WBB")[1:5]
###################

for(i in 1:length(stids)){

stid <- stids[i]
obsname <- paste0(stid,"_",start.file,"to",end.file,".RDS")
print(paste("read in",obsname))
#read in MesoWest obs (sub-hourly) output generated originally by 'readin_MesoWest.r'
DUM <- readRDS(paste0(Mdir,obsname))
dat <- DUM$dat  

#subset dataset to specified time range 
t1<-as.POSIXct(strptime(timelims[1],"%Y%m%d%H%M"),tz="UTC")
t2<-as.POSIXct(strptime(timelims[2],"%Y%m%d%H%M"),tz="UTC")
sel<-dat[,"Time"]>=t1&dat[,"Time"]<=t2
dat<-dat[sel,]

vari <- "wind_direction_set_1"; YLIMS <- c(0,360)
obs <- dat[,vari]

# Generate time series plots--use LOCAL TIME
LT<-as.POSIXlt(dat[,"Time"],tz="MST")
dev.new();par(cex.lab=1.3,cex.axis=1.1,cex.main=1.3,mar=c(5,4,4,4))
xmain <- paste(stid,"\n",timelims[1],"to",timelims[2],"[UTC]")
#xsub <- paste(timelims[1],"to",timelims[2],"[UTC]")
xsub <- DUM$STATION

#plot(LT,obs,pch=16,type="o",xlab="Time [MST]",ylab=var,main=xmain,sub=xsub,lwd=2)
plot(LT,obs,pch=16,type="o",xlab="Time [MST]",ylab=vari,
     main=xmain,sub=xsub,lwd=2,xaxt="n",cex=1.5,ylim=YLIMS)
par(new=TRUE)
vari2 <- "wind_speed_set_1"; YLIMS <- c(0,6)
plot(LT,dat[,vari2],lwd=2,axes=FALSE,col="darkgreen",pch=17,type="o",xlab="",ylab="",cex=1.5,ylim=YLIMS)
axis(4,col="darkgreen",col.axis="darkgreen")
mtext(vari2,side=4,col="darkgreen",line=2,cex=1.3)

tmp <- format(LT,"%m/%d/%Y")
#  add tick marks every day
at.tt.days <- unique(as.POSIXlt(tmp,tz="MST",format="%m/%d/%Y"))
axis.POSIXct(1,at=at.tt.days,format="%m/%d/%y",tcl=0.4)

# only add detailed tick marks for time series that stretch less than 3 days
if((as.numeric(t2)-as.numeric(t1))/3600 < (24*3)){

#  add tick marks every 12 hours
tt.min <- as.POSIXlt(min(unique(as.character(at.tt.days))),tz="MST")
tt.max <- as.POSIXlt(max(unique(as.character(at.tt.days))),tz="MST")+24*3600
axis.POSIXct(1,at=seq(tt.min,tt.max,12*3600),labels=FALSE,tcl=-0.4)
#  add tick marks every 3 hours
at.tt.3hrs <- seq(tt.min,tt.max,3*3600)
sel <- as.numeric(at.tt.3hrs)%in%as.numeric(at.tt.days) #don't plot label when date is already labelled
axis.POSIXct(1,at=at.tt.3hrs[!sel],labels=TRUE,tcl=-0.4,format="%H:%M",cex.axis=1.1)

#  add tick marks every hour
at.tt.hrly<-seq(tt.min,tt.max,3600)
if((as.numeric(t2)-as.numeric(t1))/3600 < 20){
  sel<-as.numeric(at.tt.hrly)%in%as.numeric(at.tt.3hrs)   #don't plot label when 3hrly already labelled
  axis.POSIXct(1,at=at.tt.hrly[!sel],labels=TRUE,tcl=-0.2)#add hrly labels if plot sub-daily timescales
} else {
  axis.POSIXct(1,at=at.tt.hrly,labels=FALSE,tcl=-0.2)
} # if((as.numeric(t2)-as.numeric(t1))/3600 < 20){
} # if((as.numeric(t2)-as.numeric(t1))/3600 < (24*3)){

figfilenm<-paste0(stid,"_tseries_",timelims[1],"to",timelims[2],"_wind.png")
dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"generated"))

} #for(i in 1:length(stids)){


