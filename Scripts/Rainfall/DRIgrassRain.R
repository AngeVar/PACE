#Script to analyse DRIgrass rainfall data

library(HIEv)
library(plyr)
library(plotBy)
library(doBy)
setToken(readLines("C://Repos/HIEv_token.txt"))

sortit<-function(dat){
  return(dat[order(dat$DateTime),])
}

#####################################

#Address where you want the data to be stored
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
Tm<-Tmass[c(1:20),c(1:3)]
Tm$Treatment<-droplevels(Tm$Treatment)
plotBy(Totmass~Year|Treatment, data=Tm, pch=16)

# Rainfall data from DRIgrass
Rain <- downloadTOA5("MROS_AUTO_RAIN_R", startDate="2013-01-01", endDate="2017-08-31", maxnfiles=100)
Rain$Year<- year(Rain$DateTime)
Rain$Mth<- month(Rain$DateTime)
Rain$Day<-day(Rain$DateTime)
plot(Rain$Rain_mm_Tot~Rain$Date, type='h', col="blue", ylab="Rainfall (mm)", xlab="")

# Irrigation data from DRIgrass
MROS_AUTO_IRRIG
Irrig <- downloadTOA5("MROS_AUTO_IRRIG_R", startDate="2013-01-01", endDate="2017-08-31", maxnfiles=100)
irr<-Irrig[,c(1,5:6,10)]
irr$Treatment <- recode(irr$currenttrt, 
                         "1 = 'Ambient'; 2 = 'RA'; 3 = 'IA'; 4 = 'RF'; 5 = 'SD'")
irr<-sortit(irr)
irr$Year<- year(irr$DateTime)
irr$Mth<- month(irr$DateTime)
irr$Day<-day(irr$DateTime)



########################################
#            ANALYSE DATA              #
########################################
# Rainfall data = Rain
# Total biomass = Tmass
# What period of rainfall/irrigation best describes the total mass production?



#Testing Biomass vs. the same year's rainfall
prec<-ddply(irr, .(Year,Treatment), summarise, Ann.Prec = sum(irrigsum), Days = length(Day))

DG_calendar<-merge(Tm, prec, by=c("Year","Treatment"), na.rm=T)

lm1<-lm((Totmass)~Ann.Prec, data=DG_calendar)
summary(lm1) #Crap model
plotBy((Totmass)~Ann.Prec, data=DG_calendar, pch=16, legend=F)
abline(lm1)





irr$Yrmth<-paste(irr$Year,irr$Mth, sep="-")
irr_mth<-summaryBy(irrigsum~Yrmth, data=irr, FUN= c(sum))



#Testing Biomass vs. the previous year's rainfall
prec$Prevyr<-prec$Year+1
Tm$Prevyr<-Tm$Year
DG_calendar<-merge(Tm, prec, by=c("Prevyr", "Treatment"),na.rm=T)

lm1<-lm((Totmass)~log(Ann.Prec), data=DG_calendar)
summary(lm1) #Crap model
plotBy((Totmass)~(Ann.Prec)|Treatment, data=DG_calendar, pch=16, legend=F)
abline(lm1)


#####################################
#Get calendar year annual rainfall for each year 2013-2017 (1 Jan to 31 Dec)

ann.Prec.calendar<-ddply(S1, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days = length(Day))

#####################################
#April 1 2013 to March 31 2017
S1$Mth<- month(S1$DateTime)
S1$AprYear<-ifelse(S1$Date < as.Date("2014-04-01"), "2013-2014",
                                     ifelse(S1$Date < as.Date("2015-04-01"), "2014-2015",
                                            ifelse(S1$Date < as.Date("2016-04-01"), "2015-2016", 
                                                   ifelse(S1$Date < as.Date("2017-04-01"), "2016-2017",
                                                          ifelse(S1$Date < as.Date("2018-04-01"), "2017-2018")))))

ddply(S1, .(AprYear), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################
#July 1 2013 to June 30 2017
S1$jul<-ifelse(S1$Date < as.Date("2013-07-01"), "2012-2013",
               ifelse(S1$Date < as.Date("2014-07-01"), "2013-2014",
                   ifelse(S1$Date < as.Date("2015-07-01"), "2014-2015",
                          ifelse(S1$Date < as.Date("2016-07-01"), "2015-2016", 
                                 ifelse(S1$Date < as.Date("2017-07-01"), "2016-2017",
                                        ifelse(S1$Date < as.Date("2018-07-01"), "2017-2018"))))))

ddply(S1, .(jul), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))


#####################################  
#September 1 2013 to March 31 2017

S1$SepToApr<-ifelse(S1$Date < as.Date("2013-09-01"), "",
                    ifelse(S1$Date <= as.Date("2014-03-31"), "2013-2014",
                           ifelse(S1$Date <as.Date("2014-09-01"), "",
                                  ifelse(S1$Date <= as.Date("2015-03-31"), "2014-2015",
                                         ifelse(S1$Date < as.Date("2015-09-01"), "",
                                                ifelse(S1$Date <= as.Date("2016-03-31"), "2015-2016",
                                                       ifelse(S1$Date < as.Date("2016-09-01"), "",
                                                              ifelse(S1$Date <= as.Date("2017-03-31"), "2016-2017", ""))))))))

ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################  
#November 1 2013 to March 31 2017

S1$SepToApr<-ifelse(S1$Date < as.Date("2013-11-01"), "",
                    ifelse(S1$Date <= as.Date("2014-03-31"), "2013-2014",
                           ifelse(S1$Date <as.Date("2014-11-01"), "",
                                  ifelse(S1$Date <= as.Date("2015-03-31"), "2014-2015",
                                         ifelse(S1$Date < as.Date("2015-11-01"), "",
                                                ifelse(S1$Date <= as.Date("2016-03-31"), "2015-2016",
                                                       ifelse(S1$Date < as.Date("2016-11-01"), "",
                                                              ifelse(S1$Date <= as.Date("2017-03-31"), "2016-2017", ""))))))))

ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################  
#Apr 1 2013 to Oct 31 2017

S2<- subset(S1, Mth %in% c(4:10))

ddply(S2, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))


#####################################  
#Oct 1 2013 to April 30 2017

S1$SepToApr<-ifelse(S1$Date < as.Date("2013-10-01"), "",
                    ifelse(S1$Date <= as.Date("2014-04-30"), "2013-2014",
                           ifelse(S1$Date <as.Date("2014-10-01"), "",
                                  ifelse(S1$Date <= as.Date("2015-04-30"), "2014-2015",
                                         ifelse(S1$Date < as.Date("2015-10-01"), "",
                                                ifelse(S1$Date <= as.Date("2016-04-30"), "2015-2016",
                                                       ifelse(S1$Date < as.Date("2016-10-01"), "",
                                                              ifelse(S1$Date <= as.Date("2017-04-30"), "2016-2017", ""))))))))

ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))
#####################################  
#May 1 2013 to Sep 30 2017

S1$SepToApr<-ifelse(S1$Date < as.Date("2014-05-01"), "",
                    ifelse(S1$Date <= as.Date("2014-09-30"), "2013-2014",
                           ifelse(S1$Date <as.Date("2014-05-01"), "",
                                  ifelse(S1$Date <= as.Date("2015-09-30"), "2014-2015",
                                         ifelse(S1$Date < as.Date("2015-05-01"), "",
                                                ifelse(S1$Date <= as.Date("2016-09-30"), "2015-2016",
                                                       ifelse(S1$Date < as.Date("2016-05-01"), "",
                                                              ifelse(S1$Date <= as.Date("2017-09-30"), "2016-2017", ""))))))))

ddply(S1, .(SepToApr), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))


#####################################  
#September 1 Sep to Nov 30 (Spring)

S2<- subset(S1, Mth %in% c(9:11))

ddply(S2, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################  
#Dec 1 Dec to Feb 28 (Summer)

S1$Summer<-ifelse(S1$Date < as.Date("2013-12-01"), "",
                    ifelse(S1$Date < as.Date("2014-03-01"), "2013-2014",
                           ifelse(S1$Date <as.Date("2014-12-01"), "",
                                  ifelse(S1$Date < as.Date("2015-03-01"), "2014-2015",
                                         ifelse(S1$Date < as.Date("2015-12-01"), "",
                                                ifelse(S1$Date < as.Date("2016-03-01"), "2015-2016",
                                                       ifelse(S1$Date < as.Date("2016-12-01"), "",
                                                              ifelse(S1$Date < as.Date("2017-03-01"), "2016-2017", ""))))))))

ddply(S1, .(Summer), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################  
#Dec 1 mar to May 30 (autumn)

S3<- subset(S1, Mth %in% c(3:5))

ddply(S3, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

#####################################  
#Dec 1 Jun to Aug 31 (winter)

S4<- subset(S1, Mth %in% c(6:8))

ddply(S4, .(Year), summarise, Ann.Prec = sum(Rain_mm_Tot), Days=sum(Days))

##################################### 


