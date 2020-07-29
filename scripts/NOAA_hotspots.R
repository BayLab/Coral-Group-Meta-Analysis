library(raster)
library(ncdf4)

##Here I am using annual composites from NOAA coral reef watch
##I use the hotspot data downloaded here: ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/nc/v1.0/annual
##(Not uploaded to github because it's too big)


##Read in coral locations
coralLocs <- read.csv("data/Acropora_UniqueLocations.csv")
names(coralLocs)

##Try with just one year
file="data/NOAAcrw/ct5km_hs-max_v3.1_2019.nc"
b <- brick(file)
plot(b)

##Extract data for just points
pts <- extract(b,coralLocs[,c("Longitude","Latitude")],method="bilinear")
plot(coralLocs$Longitude,pts)

##Okay, now grab all the years and ammend to coralLocs data frame
for (i in 1985:2019) {
  file=paste("data/NOAAcrw/ct5km_hs-max_v3.1_",i,".nc",sep="")
  b <- brick(file)
  pts <- extract(b,coralLocs[,c("Longitude","Latitude")],method="bilinear")
  coralLocs <- data.frame(cbind(coralLocs,pts))
  names(coralLocs)[ncol(coralLocs)]=paste("hs",i,sep="")
}


##Let's summarize this a bit
means <- rowMeans(coralLocs[,6:ncol(coralLocs)],na.rm=T) #Mean hotspot value for each location
freq1 <- apply(coralLocs[,6:ncol(coralLocs)],1,function(x) length(which(x>1))) #Number of years with hotspot >1
hsframe <- data.frame(cbind(coralLocs,hs.means=means,hs.freqs=freq1))
hsframe[which(!complete.cases(hsframe)),c("hs.means","hs.freqs")]=NA # Make sure NA gets propogated to summary statistics
write.csv(hsframe,"data/NOAA_bleaching_hotspots.csv")
