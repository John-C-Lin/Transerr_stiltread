#List of ALL possible MesoWest station IDs
#March 26th, 2019 by John C. Lin (John.Lin@utah.edu)

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
stids<-c(stids,c("USDR2","UCC51"))


