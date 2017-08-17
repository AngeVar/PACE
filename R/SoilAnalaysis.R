###########################################################################
#
#  Plot soil analysis results as heatmaps to look at spatial variation
#
###########################################################################

soil   <- read.csv("Data/PACE_soil_July2017.csv")
soil$y <- as.numeric(substr(soil$Shelter,9,9))
soil$x <- as.numeric(substr(soil$Plot,6,7))

#open a pdf
pdf("C:/Repos/PACE/Output/PACE Soil Analysis.pdf", onefile=TRUE)
par(mfrow=c(2,2), mar=c(4,4,4,5), oma=c(1,2,1,3))
dcols  <- colorRampPalette(c("White","Darkgreen"))(40)

require(akima)
resolution <- 1 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)

a      <- interp(x=data$x, y=data$y, z=data$Phosphorus.Bray.1, 
            xo=seq(min(data$x),max(data$x),by=resolution), 
            yo=seq(min(data$y),max(data$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Bray Phosphorus", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)


################################

data2 <- soil[,c("Phosphorus.Colwell","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Phosphorus.Colwell, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Colwell Phosphorous (mg/Kg)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Ammonium.Nitrogen","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Ammonium.Nitrogen, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Ammonium Nitrogen (mg/Kg)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Nitrate.Nitrogen","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Nitrate.Nitrogen, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Nitrate Nitrogen (mg/Kg)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Potassium.Colwell","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Potassium.Colwell, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

mage.plot(a, col= dcols, main="Potassium Colwell (mg/Kg)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Sulphur","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Sulphur, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Sulphur (mg/Kg)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Organic.Carbon","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Organic.Carbon, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

dcols <- colorRampPalette(c("White","Darkgreen"))(40)
image.plot(a, col= dcols, main="Organic Carbon (%)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("Conductivity","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$Conductivity, 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="Conductivity (dS/m)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("pH.Level..CaCl2.","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$pH.Level..CaCl2., 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="pH (CaCl2)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)
################################

data2 <- soil[,c("pH.Level..H2O.","x","y")]

a <- interp(x=data2$x, y=data2$y, z=data2$pH.Level..H2O., 
            xo=seq(min(data2$x),max(data2$x),by=resolution), 
            yo=seq(min(data2$y),max(data2$y),by=resolution), duplicate="mean")

image.plot(a, col= dcols, main="pH (H2O)", ylab="Shelter", xlab="Plot")
box()
abline(h=c(1.5:5.5), v=c(1.5:7.5))
segments(0,0,1.5,1.5)
segments(7.5,0.5,8.5,1.5)
segments(2.5,1.5,3.5,2.5)
segments(6.5,1.5,7.5,2.5)
segments(0.5,2.5,1.5,3.5)
segments(4.5,2.5,5.5,3.5)
segments(3.5,3.5,4.5,4.5)
segments(7.5,3.5,8.5,4.5)
segments(1.5,4.5,2.5,5.5)
segments(5.5,4.5,6.5,5.5)
segments(0.5,5.5,1.5,6.5)
segments(7.5,5.5,8.5,6.5)

dev.off()
