#Script to analyse DRIgrass rainfall data

###############################################################################
library(HIEv)
library(plyr)
library(plotBy)
library(doBy)
library(rpart)
setToken(readLines("C://Repos/HIEv_token.txt"))

#sort data by time
sortit<-function(dat){
  return(dat[order(dat$DateTime),])
}

#####################################

#Path where you want the data to be stored
dataTo<-function(){
  setwd("C:/Repos/PACE/TempData/DRIgrass_Rain")
}  
#--------------------------------------------------------------------
dataTo()
#####################################
#            LOAD DATA              #
#####################################

# Total biomass 
Tmass<-read.csv("C://Repos/PACE/Data/Rainfall/DRIGrass/DRIGrass_totmass.csv")
Tmass$Date<-as.Date(Tmass$Date)

# Irrigation data from DRIgrass
Irrig <- downloadTOA5("MROS_AUTO_IRRIG_R", startDate="2013-01-01", endDate="2017-08-31", maxnfiles=100)
irr<-Irrig[,c(1,5:6,10)]
irr$Treatment <- revalue(as.factor(irr$currenttrt),c("1" = 'Ambient', "2" = 'RA', "3" = 'IA', "4" = 'RF', "5" = 'SD'))
irr<-sortit(irr)
irr$Year<- year(irr$DateTime)
irr$Mth<- month(irr$DateTime)
irr$Day<-day(irr$DateTime)

write.csv(irr,"C:/Repos/PACE/TempData/DRIgrass_Rain/DG_irrigation.csv")

########################################
#            ANALYSE DATA              #
########################################
# Rainfall data = Rain
# Total biomass = Tmass
# Question: What period of rainfall/irrigation best describes the total mass production?


#Loop to get sum of irrigation for preceding weeks per year per treatment
################################################################################################
#split Tmass per treatments
Tm_trt<-split(Tmass, Tmass$Treatment)

prev_irr <- list()
prev_irr2<- list()
prev_irr3<- list()

#for each treatment                                                     #5 treatments
for( i in 1:length(Tm_trt)){
  
  #get the irrigation for the correct treatment
  water<-subset(irr, Treatment == Tm_trt[[i]]$Treatment[1])
  
  #then split each treatment into dates
  Trt_date<-split(Tm_trt[[i]],as.factor(Tm_trt[[i]]$Date))              #4 dates per treatment
  
  #For each date, calculate the sum of irrigation for each period preceding irrigation.
  for( j in 1:length(Trt_date)){
    
    #Add how many days to sum over; here I am looking weekly up to a year prior to harvest.
    times <- list()
    for(a in 1:52) {          #52 weeks
      times[[a]]<-(7*a)
    }
    times<- unlist(times)
    periods<-Trt_date[[j]]$Date-times     #get the dates corresponding to the set periods                                
    
    #Then, for each period, calculate the total irrigation and put in a dataframe for each date
    for( k in 1:length(periods)){
      
      sum_irr<-sum(subset(water, Date >= periods[k] & Date <= Trt_date[[j]]$Date)$irrigsum)
      prev_irr[[k]] <- data.frame(Treatment=as.factor(water$Treatment[i]),
                                  Date=as.Date(Trt_date[[j]]$Date),
                                  Days=as.factor(as.numeric(Trt_date[[j]]$Date-as.Date(periods[k]))),
                                  Irrig = sum_irr)
    }
    
    prev_irr2[[j]] <- do.call(rbind,prev_irr)
  }
  
  prev_irr3[[i]]<- do.call(rbind,prev_irr2)
}

irr_hist<-do.call(rbind,prev_irr3) #extracted irrigation histories for each treatment for each period.

#merge irrigation history with Total mass data
prod<-merge(irr_hist,Tmass, by=c("Treatment","Date"))

##############################################################################
# Now test which period of irrigation best explains the difference in Totmass across years?

hist(prod$Irrig) #not normal
hist(sqrt(prod$Irrig)) #almost normal
hist(prod$Totmass) #approximately normal

prod.l<-split(prod, as.factor(prod$Days))

output<- list()
for(m in 1:length(prod.l)){
  data<-prod.l[[m]]
  lm1<-lm((Totmass)~sqrt(Irrig), data=data)
  
  summary(lm1)
  par(mfrow=c(2,3))
  plot(residuals(lm1)~fitted.values(lm1));abline(h=0) 
  plot(lm1)
  plot(1, type="n", axes=F, xlab="", ylab="")
  output[[m]]<- data.frame(Days=prod.l[[m]]$Days[1], 
                           AIC=AIC(lm1), 
                           F=summary(lm1)$fstatistic[1],
                           df=paste(summary(lm1)$fstatistic[2],summary(lm1)$fstatistic[3], sep="/"),
                           R2=summary(lm1)$adj.r.squared,
                           p=summary(lm1)$coefficients[2,4])
}

res<- do.call(rbind,output)
res$weeks<-as.numeric(as.character(res$Days))/7
res2 <- res[order(-res$AIC),] 
write.csv(res,"C:/Repos/PACE/Output/DG_rainfall_models.csv")


plot(residuals(lm1)~fitted.values(lm1));abline(h=0)     #resid vs. fitted. Is variance approximately constant?
hist(lm1$residuals)                                     #resid normally distributed?




##############################################################################
#some plots

best<-lm((Totmass)~sqrt(Irrig), data=subset(prod, Days == 77))
plotBy(Totmass~sqrt(Irrig)|Treatment, data=subset(prod, Days == 77), 
       xlab="square root-transformed 11-week Irrigation (mm)", ylab="Total biomass (g)", 
       xlim=c(0,60),ylim=c(0,550),pch=19, cex=2, xaxs="i",yaxs="i", legend=F)
legend("topleft", legend=c("Ambient","RA","IA","RF","SD","All"),pch=c(19,19,19,19,19,NA), pt.cex = 1.5,
       lty=c(NA,NA,NA,NA,NA,1),
       col=c("black", "red","green3","blue","cyan","black"))
abline(best)
text(20,450,labels="y=4.2x + 123.3")
text(20,420,labels=expression(R^{2}~"="~0.412))

plot(best)
hist(best$residuals)  
# 
# 
# irr_day<-summaryBy(irrigsum~Treatment+Date, data=irr, FUN=sum)
# 
# plot(irrigsum.sum~Date, data=irr_day)
# 
# #Testing Biomass vs. the same year's rainfall
# prec<-ddply(irr, .(Year,Treatment), summarise, Ann.Prec = sum(irrigsum), Days = length(Day))
# 
# DG_calendar<-merge(Tm, prec, by=c("Year","Treatment"), na.rm=T)
# 
# lm1<-lm((Totmass)~Ann.Prec, data=DG_calendar)
# summary(lm1) #Crap model
# plotBy((Totmass)~Ann.Prec, data=DG_calendar, pch=16, legend=F)
# abline(lm1)
# 
# 
# 
# 
# 
# irr$Yrmth<-paste(irr$Year,irr$Mth, sep="-")
# irr_mth<-summaryBy(irrigsum~Yrmth, data=irr, FUN= c(sum))
# 
# 
# 
# #Testing Biomass vs. the previous year's rainfall
# prec$Prevyr<-prec$Year+1
# Tm$Prevyr<-Tm$Year
# DG_calendar<-merge(Tm, prec, by=c("Prevyr", "Treatment"),na.rm=T)
# 
# lm1<-lm((Totmass)~log(Ann.Prec), data=DG_calendar)
# summary(lm1) #Crap model
# plotBy((Totmass)~(Ann.Prec)|Treatment, data=DG_calendar, pch=16, legend=F)
# abline(lm1)
# 
# 
# #####################################
# #Get calendar year annual rainfall for each year 2013-2017 (1 Jan to 31 Dec)
# 
# ann.Prec.calendar<-ddply(S1, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days = length(Day))
# 
# #####################################
# #April 1 2013 to March 31 2017
# S1$Mth<- month(S1$DateTime)
# S1$AprYear<-ifelse(S1$Date < as.Date("2014-04-01"), "2013-2014",
#                                      ifelse(S1$Date < as.Date("2015-04-01"), "2014-2015",
#                                             ifelse(S1$Date < as.Date("2016-04-01"), "2015-2016", 
#                                                    ifelse(S1$Date < as.Date("2017-04-01"), "2016-2017",
#                                                           ifelse(S1$Date < as.Date("2018-04-01"), "2017-2018")))))
# 
# ddply(S1, .(AprYear), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################
# #July 1 2013 to June 30 2017
# S1$jul<-ifelse(S1$Date < as.Date("2013-07-01"), "2012-2013",
#                ifelse(S1$Date < as.Date("2014-07-01"), "2013-2014",
#                    ifelse(S1$Date < as.Date("2015-07-01"), "2014-2015",
#                           ifelse(S1$Date < as.Date("2016-07-01"), "2015-2016", 
#                                  ifelse(S1$Date < as.Date("2017-07-01"), "2016-2017",
#                                         ifelse(S1$Date < as.Date("2018-07-01"), "2017-2018"))))))
# 
# ddply(S1, .(jul), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# 
# #####################################  
# #September 1 2013 to March 31 2017
# 
# S1$SepToApr<-ifelse(S1$Date < as.Date("2013-09-01"), "",
#                     ifelse(S1$Date <= as.Date("2014-03-31"), "2013-2014",
#                            ifelse(S1$Date <as.Date("2014-09-01"), "",
#                                   ifelse(S1$Date <= as.Date("2015-03-31"), "2014-2015",
#                                          ifelse(S1$Date < as.Date("2015-09-01"), "",
#                                                 ifelse(S1$Date <= as.Date("2016-03-31"), "2015-2016",
#                                                        ifelse(S1$Date < as.Date("2016-09-01"), "",
#                                                               ifelse(S1$Date <= as.Date("2017-03-31"), "2016-2017", ""))))))))
# 
# ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################  
# #November 1 2013 to March 31 2017
# 
# S1$SepToApr<-ifelse(S1$Date < as.Date("2013-11-01"), "",
#                     ifelse(S1$Date <= as.Date("2014-03-31"), "2013-2014",
#                            ifelse(S1$Date <as.Date("2014-11-01"), "",
#                                   ifelse(S1$Date <= as.Date("2015-03-31"), "2014-2015",
#                                          ifelse(S1$Date < as.Date("2015-11-01"), "",
#                                                 ifelse(S1$Date <= as.Date("2016-03-31"), "2015-2016",
#                                                        ifelse(S1$Date < as.Date("2016-11-01"), "",
#                                                               ifelse(S1$Date <= as.Date("2017-03-31"), "2016-2017", ""))))))))
# 
# ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################  
# #Apr 1 2013 to Oct 31 2017
# 
# S2<- subset(S1, Mth %in% c(4:10))
# 
# ddply(S2, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# 
# #####################################  
# #Oct 1 2013 to April 30 2017
# 
# S1$SepToApr<-ifelse(S1$Date < as.Date("2013-10-01"), "",
#                     ifelse(S1$Date <= as.Date("2014-04-30"), "2013-2014",
#                            ifelse(S1$Date <as.Date("2014-10-01"), "",
#                                   ifelse(S1$Date <= as.Date("2015-04-30"), "2014-2015",
#                                          ifelse(S1$Date < as.Date("2015-10-01"), "",
#                                                 ifelse(S1$Date <= as.Date("2016-04-30"), "2015-2016",
#                                                        ifelse(S1$Date < as.Date("2016-10-01"), "",
#                                                               ifelse(S1$Date <= as.Date("2017-04-30"), "2016-2017", ""))))))))
# 
# ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# #####################################  
# #May 1 2013 to Sep 30 2017
# 
# S1$SepToApr<-ifelse(S1$Date < as.Date("2014-05-01"), "",
#                     ifelse(S1$Date <= as.Date("2014-09-30"), "2013-2014",
#                            ifelse(S1$Date <as.Date("2014-05-01"), "",
#                                   ifelse(S1$Date <= as.Date("2015-09-30"), "2014-2015",
#                                          ifelse(S1$Date < as.Date("2015-05-01"), "",
#                                                 ifelse(S1$Date <= as.Date("2016-09-30"), "2015-2016",
#                                                        ifelse(S1$Date < as.Date("2016-05-01"), "",
#                                                               ifelse(S1$Date <= as.Date("2017-09-30"), "2016-2017", ""))))))))
# 
# ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# 
# #####################################  
# #September 1 Sep to Nov 30 (Spring)
# 
# S2<- subset(S1, Mth %in% c(9:11))
# 
# ddply(S2, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################  
# #Dec 1 Dec to Feb 28 (Summer)
# 
# S1$Summer<-ifelse(S1$Date < as.Date("2013-12-01"), "",
#                     ifelse(S1$Date < as.Date("2014-03-01"), "2013-2014",
#                            ifelse(S1$Date <as.Date("2014-12-01"), "",
#                                   ifelse(S1$Date < as.Date("2015-03-01"), "2014-2015",
#                                          ifelse(S1$Date < as.Date("2015-12-01"), "",
#                                                 ifelse(S1$Date < as.Date("2016-03-01"), "2015-2016",
#                                                        ifelse(S1$Date < as.Date("2016-12-01"), "",
#                                                               ifelse(S1$Date < as.Date("2017-03-01"), "2016-2017", ""))))))))
# 
# ddply(S1, .(Summer), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################  
# #Dec 1 mar to May 30 (autumn)
# 
# S3<- subset(S1, Mth %in% c(3:5))
# 
# ddply(S3, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# #####################################  
# #Dec 1 Jun to Aug 31 (winter)
# 
# S4<- subset(S1, Mth %in% c(6:8))
# 
# ddply(S4, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
# 
# ##################################### 
# 
# 
