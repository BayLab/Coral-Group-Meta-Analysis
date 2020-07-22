library(raster)
library(rgdal)
library(viridis)
library(ggplot2)
library(RColorBrewer)


##Read in data from the Gridded Population of the World V4 counts. Here I have just 2015 data
path="data/gpw-v4-population-count-rev11_2015_30_sec_tif/gpw_v4_population_count_rev11_2015_30_sec.tif"
data <- raster(x="data/gpw-v4-population-count-rev11_2015_30_sec_tif/gpw_v4_population_count_rev11_2015_30_sec.tif")
breaks=c(0,1,5,25,250,1000)
plot(data,breaks=breaks,col=rev(viridis(6))) # Just try to plot the data with a reasonable distribution
crs(data) # Great! The WGS84 projection is the same as our lat/long

##Read in coral locations
coralLocs <- read.csv("data/Acropora_UniqueLocations.csv")
names(coralLocs)
points <- SpatialPointsDataFrame(coords=coralLocs[,c("Longitude","Latitude")],
                                 data=coralLocs,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(points,add=T,pch=19,cex=0.5) # make sure we can add points to the map

##Let's try to extract 50km (based on the Bruno paper) radius around each point and add together
ex  <- extract(data,points,method="simple",
               buffer=50000,
               fun=sum,
               na.rm=TRUE,
               df=TRUE)

##Plot to check
mycolors <- rev(viridis(6))
pall <- colorRampPalette(mycolors)
coralLocs$pop <- ex[,2]
coralLocs$order <- findInterval(coralLocs$pop,sort(coralLocs$pop))
plot(data,breaks=breaks,col=rev(viridis(6))) # Just try to plot the data with a reasonable distribution
points(coralLocs$Longitude,coralLocs$Latitude,
       bg=pal(nrow(coralLocs))[coralLocs$order],pch=21)
