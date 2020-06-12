##### Script #####


# Packages
install.packages("sdmpredictors")
install.packages("leaflet")
install.packages("readr")

library(sdmpredictors)
library(leaflet)

# List layers avaialble in Bio-ORACLE v2
layers.bio2 <- list_layers( datasets="Bio-ORACLE" )
layers.bio2


# Download environmental data layers (mean pH, mean Aragonite, and mean Sea Surface Salinity at the Sea Surface)
environment.coral <- load_layers( layercodes = c("BO_ph" , "BO_calcite", "BO2_salinitymean_ss") , equalarea=FALSE, rasterstack=TRUE)


# Download pH, Aragnite, SS salinity
pH <- load_layers("BO_ph")
aragonite <- load_layers("BO_calcite")
mean_ss_salinity <- load_layers("BO2_salinitymean_ss")


# Generate a data.frame with the sites of interest
library(readr)

latlongs <- read.csv("/Users/johnamielflores/Desktop/Bay Lab/Coral-Group-Meta-Analysis/data/Acropora_UniqueLocations.csv") #From the  "Acropora_UniqueLocations.csv" which may depend on your personal pathways
lat <- latlongs$Latitude
long <- latlongs$Longitude

my.sites <- data.frame(long , lat)
my.sites


# Visualise sites of interest in google maps
m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=long, lat=lat, popup=my.sites$Name)
m


# Extract environmental values from layers
my.sites.environment <- data.frame(Name=my.sites , depth=extract(pH,my.sites[1:2]) , extract(environment.coral,my.sites[1:2]) )
head.matrix(my.sites.environment)#Checking if the sites and environmenatl values align


# Load package
library(sdmpredictors)


# Easy download of raster file (Maximum Temperature at the sea bottom)
ph.mean.surface <- load_layers("BO_ph")
calcite.mean <- load_layers("BO_calcite")
salinity.mean.surface <- load_layers("BO2_salinitymean_ss")


# Crop raster to fit the Indo-Pacific, Indian OCean, Carribean
indo.pacif <- extent(75, 200, -50, 50)

ph.mean.surface.IP.crop <- crop(ph.mean.surface,indo.pacif)
calcite.mean.IP.crop <- crop(calcite.mean,indo.pacif)
salinity.mean.surface.IP.crop <- crop(salinity.mean.surface,indo.pacif)

indian <- extent(20,90,-40,35)

ph.mean.surface.I.crop <- crop(ph.mean.surface, indian)
calcite.mean.I.crop <- crop(calcite.mean, indian)
salinity.mean.surface.I.crop <- crop(salinity.mean.surface, indian)

carib <- extent(-100,-50,0,35)

ph.mean.surface.C.crop <- crop(ph.mean.surface, carib)
calcite.mean.C.crop <- crop(calcite.mean, carib)
salinity.mean.surface.C.crop <- crop(salinity.mean.surface, carib)

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))

## For Indo Pacific
plot(ph.mean.surface.IP.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface pH")

plot(calcite.mean.IP.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Calcite (mol/m)")

plot(salinity.mean.surface.IP.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface Salinity (PSS)")

## For Indian Ocean
plot(ph.mean.surface.I.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface pH")

plot(calcite.mean.I.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Calcite (mol/m)")

plot(salinity.mean.surface.I.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface Salinity (PSS)")

## For Caribbean Sea
plot(ph.mean.surface.C.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface pH")

plot(calcite.mean.C.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Calcite (mol/m)")

plot(salinity.mean.surface.C.crop,col=my.colors(1000),axes=FALSE, box=FALSE) +
  title(cex.sub = 1.25, sub = "Mean Sea Surface Salinity (PSS)")
