#This file takes raw logger data to do an initial QA/QC
#After initial set up of working directories, and tokens, 
#It uses data from the last 7 days to generate the output file then emails it to a group of addresses


#Set Token:
library(plyr)
library(HIEv)
setToken('XJZnUZvb6NRSpazmuEKw')
library(reshape2)
#library(timeDate)
library(plantecophys)
library(plotBy)
library(doBy)
library(mailR)
library(RColorBrewer)

# temporary location to store files
rm(list=ls())

#Write start date, yesterday-7 ie a week
sD<-Sys.Date()-8
#Write end date, yesterday to ensure full day of data 
eD<-Sys.Date()-1

#Address where you want the data to be stored
dataTo<-function(){
  setwd("C:/tempHIEV/PACE_WS")
}  

#Address where you want the graphs to be stored
graphsTo<-"C:/R_QA/PACE_QAchecks/"




#sD<-as.character(timeFirstDayInMonth(Sys.Date()))
#lD<-as.character(timeLastDayInMonth(Sys.Date()))
eD2<-gsub("-","",eD)
when<-paste0(eD2,".dat")
when2<-paste0(eD2,".pdf")

dataTo()
# strptime function converts date/time character strings into POSIXlt objests representing 
# date and time and saves then to dateTimes vector. dateTimes then separated into startDate
# and endDate

#####################################
#            LOAD DATA              #
#####################################

# Aboveground data
S1 <- downloadTOA5("PACE_AUTO_S1_ABVGRND_R_", startDate=sD, endDate=eD)
S2 <- downloadTOA5("PACE_AUTO_S2_ABVGRND_R_", startDate=sD, endDate=eD)
S3 <- downloadTOA5("PACE_AUTO_S3_ABVGRND_R_", startDate=sD, endDate=eD)
S4 <- downloadTOA5("PACE_AUTO_S4_ABVGRND_R_", startDate=sD, endDate=eD)
S5 <- downloadTOA5("PACE_AUTO_S5_ABVGRND_R_", startDate=sD, endDate=eD)
S6 <- downloadTOA5("PACE_AUTO_S6_ABVGRND_R_", startDate=sD, endDate=eD)
S1$shelter<-1
S2$shelter<-2
S3$shelter<-3
S4$shelter<-4
S5$shelter<-5
S6$shelter<-6
mt1<-melt(S1,id.vars=c(1,22),measure.vars=8:15)
mt2<-melt(S2,id.vars=c(1,22),measure.vars=8:15)
mt3<-melt(S3,id.vars=c(1,22),measure.vars=8:15)
mt4<-melt(S4,id.vars=c(1,22),measure.vars=8:15)
mt5<-melt(S5,id.vars=c(1,22),measure.vars=8:15)
mt6<-melt(S6,id.vars=c(1,22),measure.vars=8:15)
mt1<-rbind(mt1,mt2)
mt1<-rbind(mt1,mt3)
mt1<-rbind(mt1,mt4)
mt1<-rbind(mt1,mt5)
mt1<-rbind(mt1,mt6)

# Belowground data
S1b <- downloadTOA5("PACE_AUTO_S1_BLWGRND_R_", startDate=sD, endDate=eD)
S2b <- downloadTOA5("PACE_AUTO_S2_BLWGRND_R_", startDate=sD, endDate=eD)
S3b <- downloadTOA5("PACE_AUTO_S3_BLWGRND_R_", startDate=sD, endDate=eD)
S4b <- downloadTOA5("PACE_AUTO_S4_BLWGRND_R_", startDate=sD, endDate=eD)
S5b <- downloadTOA5("PACE_AUTO_S5_BLWGRND_R_", startDate=sD, endDate=eD)
S6b <- downloadTOA5("PACE_AUTO_S6_BLWGRND_R_", startDate=sD, endDate=eD)
S1b$shelter<-1
S2b$shelter<-2
S3b$shelter<-3
S4b$shelter<-4
S5b$shelter<-5
S6b$shelter<-6
VW1<-melt(S1b,id.vars=c(1,29),measure.vars=3:18)
VW2<-melt(S2b,id.vars=c(1,29),measure.vars=3:18)
VW3<-melt(S3b,id.vars=c(1,29),measure.vars=3:18)
VW4<-melt(S4b,id.vars=c(1,29),measure.vars=3:18)
VW5<-melt(S5b,id.vars=c(1,29),measure.vars=3:18)
VW6<-melt(S6b,id.vars=c(1,29),measure.vars=3:18)
VW<-rbind(VW1,VW2)
VW<-rbind(VW,VW3)
VW<-rbind(VW,VW4)
VW<-rbind(VW,VW5)
VW<-rbind(VW,VW6)

ST1<-melt(S1b,id.vars=c(1,29),measure.vars=19:26)
ST2<-melt(S2b,id.vars=c(1,29),measure.vars=19:26)
ST3<-melt(S3b,id.vars=c(1,29),measure.vars=19:26)
ST4<-melt(S4b,id.vars=c(1,29),measure.vars=19:26)
ST5<-melt(S5b,id.vars=c(1,29),measure.vars=19:26)
ST6<-melt(S6b,id.vars=c(1,29),measure.vars=19:26)
ST<-rbind(ST1,ST2)
ST<-rbind(ST,ST3)
ST<-rbind(ST,ST4)
ST<-rbind(ST,ST5)
ST<-rbind(ST,ST6)

#####################################
#            DRAW GRAPHS            #
#####################################
filenme<-paste0(graphsTo,"PACE_QA_",when2)
pdf(filenme, onefile=TRUE)


palette(brewer.pal(8, "Dark2"))


ymax<-max(mt1$value,ST$value) #use same temperature range across temperature charts
ymin<-min(mt1$value,ST$value)
xmax<-max(mt1$DateTime)
xmin<-min(mt1$DateTime)

par(mfrow=c(3,1))
#plot battery voltages
plot(S1$batt_volt_Min~S1$DateTime,col=1,type='l',main="Battery Voltage",ylim=c(10,15))
points(S2$batt_volt_Min~S2$DateTime,type='l',col=2)
points(S3$batt_volt_Min~S3$DateTime,type='l',col=3)
points(S4$batt_volt_Min~S4$DateTime,type='l',col=4)
points(S5$batt_volt_Min~S5$DateTime,type='l',col=5)
points(S6$batt_volt_Min~S6$DateTime,type='l',col=6)
grid()
mtext("-1-                           ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                      ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                 ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-            ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-       ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-  ",side=3,line=0,col=6,cex=0.8)


#Tair
plot(S1$AirT1_Avg~S1$DateTime,col=1,type='l',main="Air Temp",ylim=c(0,40))
points(S2$AirT1_Avg~S2$DateTime,type='l',col=2)
points(S3$AirT1_Avg~S3$DateTime,type='l',col=3)
points(S4$AirT1_Avg~S4$DateTime,type='l',col=4)
points(S5$AirT1_Avg~S5$DateTime,type='l',col=5)
points(S6$AirT1_Avg~S6$DateTime,type='l',col=6)
grid()
mtext("-1-                           ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                      ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                 ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-            ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-       ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-  ",side=3,line=0,col=6,cex=0.8)

#Tair-diff
plot(S1$AirT1_Avg-S1$AirT2~S1$DateTime,col=1,type='l',main="Air Temp Diff")
points(S2$AirT1_Avg-S1$AirT2_Avg~S2$DateTime,type='l',col=2)
points(S3$AirT1_Avg-S3$AirT2_Avg~S3$DateTime,type='l',col=3)
points(S4$AirT1_Avg-S4$AirT2_Avg~S4$DateTime,type='l',col=4)
points(S5$AirT1_Avg-S5$AirT2_Avg~S5$DateTime,type='l',col=5)
points(S6$AirT1_Avg-S6$AirT2_Avg~S6$DateTime,type='l',col=6)
grid()
mtext("-1-                           ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                      ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                 ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-            ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-       ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-  ",side=3,line=0,col=6,cex=0.8)
mtext(paste0(round(range(S1$AirT1_Avg-S1$AirT2_Avg)[1],1),":",round(range(S1$AirT1_Avg-S1$AirT2_Avg)[2],1)),side=1,line=2,col=1,cex=0.8,adj=0)
mtext(paste0(round(range(S2$AirT1_Avg-S2$AirT2_Avg)[1],1),":",round(range(S2$AirT1_Avg-S2$AirT2_Avg)[2],1)),side=1,line=2,col=2,cex=0.8,adj=0.2)
mtext(paste0(round(range(S3$AirT1_Avg-S3$AirT2_Avg)[1],1),":",round(range(S3$AirT1_Avg-S3$AirT2_Avg)[2],1)),side=1,line=2,col=3,cex=0.8,adj=0.4)
mtext(paste0(round(range(S4$AirT1_Avg-S4$AirT2_Avg)[1],1),":",round(range(S4$AirT1_Avg-S4$AirT2_Avg)[2],1)),side=1,line=2,col=4,cex=0.8,adj=0.6)
mtext(paste0(round(range(S5$AirT1_Avg-S5$AirT2_Avg)[1],1),":",round(range(S5$AirT1_Avg-S5$AirT2_Avg)[2],1)),side=1,line=2,col=5,cex=0.8,adj=0.8)
mtext(paste0(round(range(S6$AirT1_Avg-S6$AirT2_Avg)[1],1),":",round(range(S6$AirT1_Avg-S6$AirT2_Avg)[2],1)),side=1,line=2,col=6,cex=0.8,adj=1)


#PAGE 4 
#PAR and RH
par(mfrow=c(2,1))
#PAR
plot(S1$PAR_Avg~S1$DateTime,col=1,type='l',main="PAR",ylim=c(0,max(S1$PAR_Avg,S2$PAR_Avg,S3$PAR_Avg,S4$PAR_Avg,S5$PAR_Avg,S6$PAR_Avg,na.rm=T)))
points(S2$PAR_Avg~S2$DateTime,type='l',col=2)
points(S3$PAR_Avg~S3$DateTime,type='l',col=3)
points(S4$PAR_Avg~S4$DateTime,type='l',col=4)
points(S5$PAR_Avg~S5$DateTime,type='l',col=5)
points(S6$PAR_Avg~S6$DateTime,type='l',col=6)
grid()
mtext("-1-                           ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                      ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                 ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-            ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-       ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-  ",side=3,line=0,col=6,cex=0.8)

#RH
plot(S1$RH_Avg~S1$DateTime,col=1,type='l',main="RH",ylim=c(0,100))
points(S2$RH_Avg~S2$DateTime,type='l',col=2)
points(S3$RH_Avg~S3$DateTime,type='l',col=3)
points(S4$RH_Avg~S4$DateTime,type='l',col=4)
points(S5$RH_Avg~S5$DateTime,type='l',col=5)
points(S6$RH_Avg~S6$DateTime,type='l',col=6)
grid()
mtext("-1-                           ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                      ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                 ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-            ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-       ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-  ",side=3,line=0,col=6,cex=0.8)



plot_BodyT<-function(shltr){
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("IR Body T S:",shltr),ylim=c(ymin,ymax),xlim=c(xmin,xmax)))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.2."),],points(value~DateTime,type='l',col=2,))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.3."),],points(value~DateTime,type='l',col=3,))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.4."),],points(value~DateTime,type='l',col=4,))
  grid()
  mtext("1                ",side=2,line=2,col=1,cex=0.7)
  mtext("    2            ",side=2,line=2,col=2,cex=0.7)
  mtext("        3        ",side=2,line=2,col=3,cex=0.7)
  mtext("            4    ",side=2,line=2,col=4,cex=0.7)
  }



par(mfrow=c(3,2))
#plot body temperatures for each shelter by plotnumber
for (i in 1:6) {
  plot_BodyT(i)
}

plot_TargetT<-function(shltr){
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("IR Targ T S:",shltr),ylim=c(ymin,ymax),xlim=c(xmin,xmax)))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.2."),],points(value~DateTime,type='l',col=2,))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.3."),],points(value~DateTime,type='l',col=3,))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.4."),],points(value~DateTime,type='l',col=4,))
  grid()
  mtext("1                ",side=2,line=2,col=1,cex=0.7)
  mtext("    2            ",side=2,line=2,col=2,cex=0.7)
  mtext("        3        ",side=2,line=2,col=3,cex=0.7)
  mtext("            4    ",side=2,line=2,col=4,cex=0.7)
}



par(mfrow=c(3,2))
#plot body temperatures for each shelter by plotnumber
for (i in 1:6) {
  plot_TargetT(i)
}


#PAGE 5
#soil temperature sensors

plot_SoilT<-function(shltr){
 
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("Soil Temp S:",shltr),ylim=c(ymin,ymax),xlim=c(xmin,xmax)))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.2."),],points(value~DateTime,type='l',col=2,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.3."),],points(value~DateTime,type='l',col=3,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.4."),],points(value~DateTime,type='l',col=4,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.5."),],points(value~DateTime,type='l',col=5,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.6."),],points(value~DateTime,type='l',col=6,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.7."),],points(value~DateTime,type='l',col=7,))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.8."),],points(value~DateTime,type='l',col=8,))
  grid()
  
  mtext("1",side=1,line=2,col=1,cex=0.7,adj=0)
  mtext("2",side=1,line=2,col=2,cex=0.7,adj=0.14)
  mtext("3",side=1,line=2,col=3,cex=0.7,adj=0.28)
  mtext("4",side=1,line=2,col=4,cex=0.7,adj=0.43)  
  mtext("5",side=1,line=2,col=5,cex=0.7,adj=0.57) 
  mtext("6",side=1,line=2,col=6,cex=0.7,adj=0.71)  
  mtext("7",side=1,line=2,col=7,cex=0.7,adj=0.85)  
  mtext("8",side=1,line=2,col=8,cex=0.7,adj=1)
  
}
par(mfrow=c(3,2))
#plot body temperatures for each shelter by plotnumber
for (i in 1:6) {
  plot_SoilT(i)
}
dev.off()

###########################################################################################################

