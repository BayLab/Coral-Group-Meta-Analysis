# Human Urban population data

#install packages####
install.packages('raster')
install.packages('sp')
install.packages('leaflet')
library(raster)
library(sp)
library(leaflet)
library(readr)
library(dplyr)
library(spData)
library(lubridate)
library(ggplot2)
library(data.table)
library(tidyverse)

#create world map####
download.packages("maps")
library(maps)
Acropora_UniqueLocations <- read.csv("data/Acropora_UniqueLocations.csv")
View(Acropora_UniqueLocations)
attach(Acropora_UniqueLocations)

data("state.vbmMapEnv")
data("state.vbm.center")
world <- map_data("world")

library(ggplot2)
ggplot(data=world) + geom_polygon(aes(x = long, y = lat, group=group), fill="white", color = "gray") + coord_fixed(1.3) +guides(fill=FALSE)
                                                                              

#add coral data points####
ggplot(data=world) + geom_polygon(aes(x = long, y = lat, group=group), fill="white", color = "gray") + coord_fixed(1.3) +guides(fill=FALSE)+guides(fill=FALSE)+geom_point(data=Acropora_UniqueLocations, aes(x=Longitude, y=Latitude))

#now to overlay population data####
library(rgdal)
cities<-readOGR(system.file("vectors", package="rgdal")[1], "cities")

#geopackage format
outname<-"urban.gpkg"
writeOGR(cities, dsn=outname, layer="cities", driver="GPKG")

urban_gpkg <-readOGR("urban.gpkg", "cities")
identical(cities, urban_gpkg) #[1] TRUE
head(urban_gpkg)

ggplot(urban_gpkg)

library(dplyr)
cities_sqlite<-tbl(src_sqlite("urban2.gpkg"),"cities")
print(cities_sqlite, n=30)
View(cities_sqlite)

# Data formatting & manipulation ----
# Simplify occurrence data frame
DEN <- cities_sqlite %>% dplyr::select(geom, NAME, COUNTRY, POPULATION)
DEN
View(DEN)


#make ggplot an object
coralpops <-ggplot(data=world) + geom_polygon(aes(x = long, y = lat, group=group), fill="white", color = "gray") + coord_fixed(1.3) +guides(fill=FALSE)+guides(fill=FALSE)+geom_point(data=Acropora_UniqueLocations, aes(x=Longitude, y=Latitude))
coralpops                                                                                        
all<-coralpops 

