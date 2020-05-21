library(tidyverse)
library(rworldmap)

##Read in Excel spreadsheet
d <- read.csv("data/GeneticData_Acropora_cleaned.csv")

##Give each location a unique identifier
d$loc <- paste(d$Number,d$Latitude,d$Longitude,sep="_")

##How many locations per species? A location is a unique lat/long from a single study
t <- table(d$loc,d$Species)
loc.num <- apply(t,2,function(x) length(which(x>0)))

##How many rows per species?
row.num <- table(d$Species)

##Put those two frames together
summary <- data.frame(loc.num,row.num)[,c(1,3)]

##Map locations for all species
plot(d$Longitude,d$Latitude,pch=19,col="red",xlab="",ylab="")
mymap <- getMap(resolution = "low")
plot(mymap,add=T,lwd=1.5,col="grey90")


##Output lists of unique locations and species
write.csv(summary,"Acropora_SpeciesList.csv")
locations <- d %>%
  group_by(loc) %>%
  summarise(Number=mean(Number),
            Longitude=mean(Longitude),
            Latitude=mean(Latitude))
write.csv(locations,"Acropora_UniqueLocations.csv")
