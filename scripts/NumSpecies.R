####This species uses the ecoregion to provide a regional species richness estimate
library(rgdal)
library(ggplot2)

###Get the ecoregions and species numbers downloaded from https://hub.arcgis.com/datasets/8e63b2db726b4a5ca7428b3cc9305c7f_14/data
species <- readOGR("~/Downloads/Number_of_Stony_Coral_Species-shp/",layer="Number_of_Stony_Coral_Species")

###our points
points <- read.csv("data/MasterDataFrame.csv")
spts <- SpatialPointsDataFrame(coords=points[,c("Longitude","Latitude")],data=points,proj4string=crs(species))

points$species <- over(spts,species)$coral_spcs

plot(points$He.all~points$species)


p <- ggplot(points[points$Ocean=="Pacific",],aes(x=species,y=adj.He,color=Ocean)) +
  geom_point() + 
  stat_smooth(aes(group=1),method="lm",color="black") +
  theme_bw()
p

write.csv(points,"data/MasterDataFrame.csv")
