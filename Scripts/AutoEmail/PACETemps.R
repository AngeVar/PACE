#This file takes raw logger data to do an initial QA/QC
#After initial set up of working directories, and tokens, 
#It uses data from the last 7 days to generate the output file then emails it to a group of addresses


#Set Token:
library(plyr)
library(HIEv)
setToken(readLines("C:/Repos/HIEv_token.txt"))
library(reshape2)
library(timeDate)
library(plantecophys)
library(plotBy)
library(doBy)
library(mailR)
library(RColorBrewer)
library(ggplot2)

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

sortit<-function(dat){
  return(dat[order(dat$DateTime),])
}
#read in treatments
trts<-read.csv("C:/autoscripts/treatments.csv",sep=",")

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
S1<-sortit(S1)
S2 <- downloadTOA5("PACE_AUTO_S2_ABVGRND_R_", startDate=sD, endDate=eD)
S2<-sortit(S2)
S3 <- downloadTOA5("PACE_AUTO_S3_ABVGRND_R_", startDate=sD, endDate=eD)
S3<-sortit(S3)
S4 <- downloadTOA5("PACE_AUTO_S4_ABVGRND_R_", startDate=sD, endDate=eD)
S4<-sortit(S4)
S5 <- downloadTOA5("PACE_AUTO_S5_ABVGRND_R_", startDate=sD, endDate=eD)
S5<-sortit(S5)
S6 <- downloadTOA5("PACE_AUTO_S6_ABVGRND_R_", startDate=sD, endDate=eD)
S6<-sortit(S6)

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


#heater duty cycle
DC1<-sortit(downloadTOA5("PACE_AUTO_S1_DUTYCYCLE_R_", startDate=sD, endDate=eD))
DC2<-sortit(downloadTOA5("PACE_AUTO_S2_DUTYCYCLE_R_", startDate=sD, endDate=eD))
DC3<-sortit(downloadTOA5("PACE_AUTO_S3_DUTYCYCLE_R_", startDate=sD, endDate=eD))
DC4<-sortit(downloadTOA5("PACE_AUTO_S4_DUTYCYCLE_R_", startDate=sD, endDate=eD))
DC5<-sortit(downloadTOA5("PACE_AUTO_S5_DUTYCYCLE_R_", startDate=sD, endDate=eD))
DC6<-sortit(downloadTOA5("PACE_AUTO_S6_DUTYCYCLE_R_", startDate=sD, endDate=eD))

DC1$shelter<-1
DC2$shelter<-1
DC3$shelter<-1
DC4$shelter<-1
DC5$shelter<-1
DC6$shelter<-1
DC1m<-melt(DC1,id.vars=c(1,9),measure.vars=3:6)
DC2m<-melt(DC2,id.vars=c(1,9),measure.vars=3:6)
DC3m<-melt(DC3,id.vars=c(1,9),measure.vars=3:6)
DC4m<-melt(DC4,id.vars=c(1,9),measure.vars=3:6)
DC5m<-melt(DC5,id.vars=c(1,9),measure.vars=3:6)
DC6m<-melt(DC6,id.vars=c(1,9),measure.vars=3:6)
DC<-rbind(DC1m,DC2m)
DC<-rbind(DC,DC3m)
DC<-rbind(DC,DC4m)
DC<-rbind(DC,DC5m)
DC<-rbind(DC,DC6m)

# Belowground data
S1b <- sortit(downloadTOA5("PACE_AUTO_S1_BLWGRND_R_", startDate=sD, endDate=eD))
S2b <- sortit(downloadTOA5("PACE_AUTO_S2_BLWGRND_R_", startDate=sD, endDate=eD))
S3b <- sortit(downloadTOA5("PACE_AUTO_S3_BLWGRND_R_", startDate=sD, endDate=eD))
S4b <- sortit(downloadTOA5("PACE_AUTO_S4_BLWGRND_R_", startDate=sD, endDate=eD))
S5b <- sortit(downloadTOA5("PACE_AUTO_S5_BLWGRND_R_", startDate=sD, endDate=eD))
S6b <- sortit(downloadTOA5("PACE_AUTO_S6_BLWGRND_R_", startDate=sD, endDate=eD))
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
VW$sensor<-as.numeric(gsub("^VW_Avg*\\.", "", VW$variable))
VW$ID<-(VW$shelter-1)*8+trunc(VW$sensor/2+0.5,0) #plot 1-48 in which that sensor sits
VWx<-merge(VW,trts,by="ID")
VWx$Warm<-VWx$PLOT %in% c("3","4","5","6") #discriminate plots in the warming experiment
lastVW<-subset(VWx,DateTime==max(VWx$DateTime))
lastVW$trt<-interaction(lastVW$Warm,lastVW$Temp,lastVW$Rain)
lastVW<-lastVW[order(lastVW$Warm),]
lastVW$trt<-factor(lastVW$trt, levels=c("FALSE.Amb.Con", "FALSE.Amb.Drt", "TRUE.Amb.Con","TRUE.eT.Con","TRUE.Amb.Drt","TRUE.eT.Drt","FALSE.eT.Con","FALSE.eT.Drt"))


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

#irrigation data
irr<-sortit(downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD))

#Windspeed data
wind<-sortit(downloadTOA5("PACE_AUTO_ALL_WINDSPEED_R_", startDate=sD, endDate=eD))

#ROS weather station data for $AirTC_Avg $PPFD_Avg
ROS05min <- sortit(downloadTOA5("ROS_WS_Table05min_", startDate=sD, endDate=eD))

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
plot(S1$batt_volt_Min~S1$DateTime,col=1,type='l',main="Battery Voltage",ylim=c(10,15), ylab="V", xlab="")
axis(4, at=c(10:15), labels = c(10,"",12,"",14,""))
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
plot(S1$AirT1_Avg~S1$DateTime,col=1,type='l',main="Air Temp",ylim=c(0,35), ylab=expression(~degree~C), xlab="")
axis(4, at=seq(0,35,by=5), labels = seq(0,35,by=5))
points(S2$AirT1_Avg~S2$DateTime,type='l',col=2)
points(S3$AirT1_Avg~S3$DateTime,type='l',col=3)
points(S4$AirT1_Avg~S4$DateTime,type='l',col=4)
points(S5$AirT1_Avg~S5$DateTime,type='l',col=5)
points(S6$AirT1_Avg~S6$DateTime,type='l',col=6)
points(ROS05min$AirTC_Avg~ROS05min$DateTime, type='l',col=7)
grid()
mtext("-1-                                  ",side=3,line=0,col=1,cex=0.8)
mtext("     -2-                             ",side=3,line=0,col=2,cex=0.8)
mtext("          -3-                        ",side=3,line=0,col=3,cex=0.8)
mtext("               -4-                   ",side=3,line=0,col=4,cex=0.8)
mtext("                    -5-              ",side=3,line=0,col=5,cex=0.8)
mtext("                         -6-         ",side=3,line=0,col=6,cex=0.8)
mtext("                                  ROS",side=3,line=0,col=7,cex=0.8)


#Tair-diff
plot(S1$AirT1_Avg-S1$AirT2~S1$DateTime,col=1,type='l',main="Air Temp Diff",ylab="T1-T2",xlab="",ylim=c(-1,1.5))
axis(4, at=seq(-1.0,1.5,by=0.5),labels=c("-1.0","-0.5","0.0","0.5","1.0","1.5"))
points(S2$AirT1_Avg-S2$AirT2_Avg~S2$DateTime,type='l',col=2)
points(S3$AirT1_Avg-S3$AirT2_Avg~S3$DateTime,type='l',col=3)
points(S4$AirT1_Avg-S4$AirT2_Avg~S4$DateTime,type='l',col=4)
points(S5$AirT1_Avg-S5$AirT2_Avg~S5$DateTime,type='l',col=5)
points(S6$AirT1_Avg-S6$AirT2_Avg~S6$DateTime,type='l',col=6)
grid()
mtext("1-out                                                                ",side=3,line=0,col=1,cex=0.8)
mtext("             2-in                                                    ",side=3,line=0,col=2,cex=0.8)
mtext("                         3-out                                       ",side=3,line=0,col=3,cex=0.8)
mtext("                                      4-in                           ",side=3,line=0,col=4,cex=0.8)
mtext("                                                  5-out              ",side=3,line=0,col=5,cex=0.8)
mtext("                                                               6-in  ",side=3,line=0,col=6,cex=0.8)
mtext(paste0(round(range(S1$AirT1_Avg-S1$AirT2_Avg)[1],1),":",round(range(S1$AirT1_Avg-S1$AirT2_Avg)[2],1)),side=1,line=2.5,col=1,cex=0.8,adj=0)
mtext(paste0(round(range(S2$AirT1_Avg-S2$AirT2_Avg)[1],1),":",round(range(S2$AirT1_Avg-S2$AirT2_Avg)[2],1)),side=1,line=2.5,col=2,cex=0.8,adj=0.2)
mtext(paste0(round(range(S3$AirT1_Avg-S3$AirT2_Avg)[1],1),":",round(range(S3$AirT1_Avg-S3$AirT2_Avg)[2],1)),side=1,line=2.5,col=3,cex=0.8,adj=0.4)
mtext(paste0(round(range(S4$AirT1_Avg-S4$AirT2_Avg)[1],1),":",round(range(S4$AirT1_Avg-S4$AirT2_Avg)[2],1)),side=1,line=2.5,col=4,cex=0.8,adj=0.6)
mtext(paste0(round(range(S5$AirT1_Avg-S5$AirT2_Avg)[1],1),":",round(range(S5$AirT1_Avg-S5$AirT2_Avg)[2],1)),side=1,line=2.5,col=5,cex=0.8,adj=0.8)
mtext(paste0(round(range(S6$AirT1_Avg-S6$AirT2_Avg)[1],1),":",round(range(S6$AirT1_Avg-S6$AirT2_Avg)[2],1)),side=1,line=2.5,col=6,cex=0.8,adj=1)


#PAGE 4 
#PAR and RH
par(mfrow=c(2,1))
#PAR
plot(S1$PAR_Avg~S1$DateTime,col=1,type='l',main="PAR",
     ylim=c(0,max(S1$PAR_Avg,S2$PAR_Avg,S3$PAR_Avg,S4$PAR_Avg,S5$PAR_Avg,S6$PAR_Avg,na.rm=T)),xlab="",ylab="")
mtext(side=2, line=2.5,text=expression(paste(mu,"mol",m^-2~s^-1)))
axis(4, at=c(0,500,1000,1500),labels = c(0,500,1000,1500))
points(S2$PAR_Avg~S2$DateTime,type='l',col=2)
points(S5$PAR_Avg~S5$DateTime,type='l',col=5)
points(S6$PAR_Avg~S6$DateTime,type='l',col=6)
points(ROS05min$PPFD_Avg~ROS05min$DateTime,type='l',col=7)
grid()
mtext("1-in                                                           ",side=3,line=0,col=1,cex=0.8)
mtext("            2-out                                              ",side=3,line=0,col=2,cex=0.8)
mtext("                              5-in                             ",side=3,line=0,col=5,cex=0.8)
mtext("                                              6-out            ",side=3,line=0,col=6,cex=0.8)
mtext("                                                            ROS",side=3,line=0,col=7,cex=0.8)

#RH
plot(S1$RH_Avg~S1$DateTime,col=1,type='l',main="RH",ylim=c(0,100), xlab="", ylab="%")
axis(4, at=c(0,20,40,60,80,100), labels = c(0,20,40,60,80,100))
points(S2$RH_Avg~S2$DateTime,type='l',col=2)
points(S3$RH_Avg~S3$DateTime,type='l',col=3)
points(S4$RH_Avg~S4$DateTime,type='l',col=4)
points(S5$RH_Avg~S5$DateTime,type='l',col=5)
points(S6$RH_Avg~S6$DateTime,type='l',col=6)
points(ROS05min$RH~ROS05min$DateTime,type='l',col=7)
grid()
mtext("1-out                                                                         ",side=3,line=0,col=1,cex=0.8)
mtext("             2-in                                                             ",side=3,line=0,col=2,cex=0.8)
mtext("                         3-out                                                ",side=3,line=0,col=3,cex=0.8)
mtext("                                      4-in                                    ",side=3,line=0,col=4,cex=0.8)
mtext("                                                  5-out                       ",side=3,line=0,col=5,cex=0.8)
mtext("                                                               6-in           ",side=3,line=0,col=6,cex=0.8)
mtext("                                                                           ROS",side=3,line=0,col=7,cex=0.8)


plot_BodyT<-function(shltr){
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("IR Body T S:",shltr),ylim=c(ymin,ymax),xlim=c(xmin,xmax)))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.2."),],points(value~DateTime,type='l',col=2))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.3."),],points(value~DateTime,type='l',col=3))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="SBTemp_Avg.4."),],points(value~DateTime,type='l',col=4))
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
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.2."),],points(value~DateTime,type='l',col=2))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.3."),],points(value~DateTime,type='l',col=3))
  with(mt1[which(mt1$shelter==shltr & mt1$variable=="TargTemp_Avg.4."),],points(value~DateTime,type='l',col=4))
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
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.2."),],points(value~DateTime,type='l',col=2))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.3."),],points(value~DateTime,type='l',col=3))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.4."),],points(value~DateTime,type='l',col=4))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.5."),],points(value~DateTime,type='l',col=5))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.6."),],points(value~DateTime,type='l',col=6))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.7."),],points(value~DateTime,type='l',col=7))
  with(ST[which(ST$shelter==shltr & ST$variable=="TSoil_Avg.8."),],points(value~DateTime,type='l',col=8))
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

#Heater Duty Cycle
plot_DutyCycle<-function(shltr){
  
  with(DC[which(DC$shelter==shltr & DC$variable=="DutyCycle_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("DutyCycle S:",shltr),ylim=c(-0.1,1.1),xlim=c(xmin,xmax)))
  with(DC[which(DC$shelter==shltr & DC$variable=="DutyCycle_Avg.2."),],points(value~DateTime,type='l',col=2))
  grid()
  
  mtext("1",side=1,line=2,col=1,cex=0.7,adj=0.3)
  mtext("2",side=1,line=2,col=2,cex=0.7,adj=0.7)
 
  
}
par(mfrow=c(3,2))
#plot body temperatures for each shelter by plotnumber
for (i in 1:6) {
  plot_DutyCycle(i)
}


#soil water

plot_VW<-function(shltr){
  
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.1."),],plot(value~DateTime,type='l',col=1,main=paste0("VW S:",shltr),ylim=c(VWmin,VWmax),xlim=c(xmin,xmax)))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.2."),],points(value~DateTime,type='l',col=2))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.3."),],points(value~DateTime,type='l',col=3))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.4."),],points(value~DateTime,type='l',col=4))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.5."),],points(value~DateTime,type='l',col=5))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.6."),],points(value~DateTime,type='l',col=6))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.7."),],points(value~DateTime,type='l',col=7))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.8."),],points(value~DateTime,type='l',col=8))
  grid()
  
  mtext("1",side=1,line=2,col=1,cex=0.7,adj=0)
  mtext("2",side=1,line=2,col=2,cex=0.7,adj=0.14)
  mtext("3",side=1,line=2,col=3,cex=0.7,adj=0.28)
  mtext("4",side=1,line=2,col=4,cex=0.7,adj=0.43)  
  mtext("5",side=1,line=2,col=5,cex=0.7,adj=0.57) 
  mtext("6",side=1,line=2,col=6,cex=0.7,adj=0.71)  
  mtext("7",side=1,line=2,col=7,cex=0.7,adj=0.85)  
  mtext("8",side=1,line=2,col=8,cex=0.7,adj=1)
  
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.9."),],plot(value~DateTime,type='l',col=1,main=paste0("VW S:",shltr),ylim=c(VWmin,VWmax),xlim=c(xmin,xmax)))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.10."),],points(value~DateTime,type='l',col=2))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.11."),],points(value~DateTime,type='l',col=3))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.12."),],points(value~DateTime,type='l',col=4))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.13."),],points(value~DateTime,type='l',col=5))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.14."),],points(value~DateTime,type='l',col=6))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.15."),],points(value~DateTime,type='l',col=7))
  with(VW[which(VW$shelter==shltr & VW$variable=="VW_Avg.16."),],points(value~DateTime,type='l',col=8))
  grid()
  
  mtext("9",side=1,line=2,col=1,cex=0.7,adj=0)
  mtext("10",side=1,line=2,col=2,cex=0.7,adj=0.14)
  mtext("11",side=1,line=2,col=3,cex=0.7,adj=0.28)
  mtext("12",side=1,line=2,col=4,cex=0.7,adj=0.43)  
  mtext("13",side=1,line=2,col=5,cex=0.7,adj=0.57) 
  mtext("14",side=1,line=2,col=6,cex=0.7,adj=0.71)  
  mtext("15",side=1,line=2,col=7,cex=0.7,adj=0.85)  
  mtext("16",side=1,line=2,col=8,cex=0.7,adj=1)
  
}
par(mfrow=c(3,2))
VWmax<-max(VW$value,na.rm=T)
VWmin<-min(VW$value,na.rm=T)
#plot body temperatures for each shelter by plotnumber
for (i in 1:6) {
  plot_VW(i)
}

qplot(trt,value,data=lastVW,geom=c("boxplot", "point"),main="VW by treatment (TRUE=warming exp)")


dev.off()

# ###########################################################################################################
# ##email results to interested parties
# 
# #debug(send.mail)
# #undebug(send.mail)
# 
# bdy<-paste("Data from PACE for ",sD," to ",eD)
# send.mail(from = "CUP.FLUX@gmail.com",
#           to = c("c.barton@westernsydney.edu.au","a.varhammar@westernsydney.edu.au","b.amiji@westernsydney.edu.au"),#"e.pendall@uws.edu.au","c.maier@uws.edu.au","a.renchon@uws.edu.au"),
#           subject = "PACE last week data",
#           body = paste("Data for ",sD," to ",eD),
#           smtp = list(host.name = "smtp.gmail.com",port = 587,user.name = "CUP.FLUX", passwd = "HIEFlux88", ssl = TRUE),
#           authenticate = TRUE,
#           send = TRUE,
#           attach.files = c(filenme),
#           #file.names = c("Download log.log", "Upload log.log", "DropBox File.rtf"), # optional parameter
#           #file.descriptions = c("Description for download log", "Description for upload log", "DropBox File"), # optional parameter
#           debug = FALSE)
# 
# 
# plotBy(irrigrate~Date|as.factor(irr$valveNo), data=irr, legend=F) #5mm takes ~467 seconds = 7.8 min
# grid()
# low<-subset(irr, Date>as.Date("2017-09-01") & elapsed<250)
# ddply(low, c("Date", "valveNo"), summarise,mean = mean(elapsed))
# 
# plot(WS_ms_Avg~DateTime, data=wind)
# wind$Hour<-hour(wind$DateTime)
# night_wind<-subset(wind, Hour <=8 | Hour >=20)
# night1<-subset(night_wind, Date== as.Date("2017-09-18"))
# plot(WS_ms_Avg~DateTime, data=night1)
