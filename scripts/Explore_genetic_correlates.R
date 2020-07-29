library(dplyr)
library(ggplot2)
library(randomForest)

##This script is means to help you combine the genetic data with your seascape/trait data

#First, read in the genetic data
gen <- read.csv("data/Heterozygosity_fixed_07.28.20.csv")

#Now read in your data - here I'm using data I summarized from NOAA hotspots
hotspots <- read.csv("data/NOAA_bleaching_hotspots.csv")

##the 'loc' column can be used to merge these two dataframes
##If you are using species traits you may need to merge by the 'species' column
merge <- merge(gen,hotspots,by.x=c("loc","Latitude","Longitude","Number"),by.y=c("loc","Latitude","Longitude","Number"))



##Try random forest - this is an exploratory way to see which variables might be important
imp.vars <- c("He.all","Species","Ocean",
              "Latitude","Longitude","Sample.year",
              "Number","Cross.species","Primer.note","hs.freqs","hs.means") # You shouldn't have to use adjust He here
sub <- merge[,imp.vars]
nona <- na.omit(sub) # This framework doesn't handle missing data so the the more variables you include the more data you lose
dim(nona)
rf <- randomForest(He.all~.,data=nona)
rf # We can see % Variance explained here
varImpPlot(rf)


##Some variable are going to be colinear. You can look at this here:
#Make sure to only include numeric factors
pairs(nona[,c("Longitude","Latitude","hs.freqs","hs.means","Sample.year")]) #hs.freqs and hs.means are pretty correlated - probably don't need them both


##If you want to look at just one variable
p <- ggplot(merge,aes(x=hs.means)) +
  geom_point(aes(y=adj.He)) + 
  stat_smooth(aes(y=adj.He),method="lm") +
  theme_bw()
p

##If you want to look in just one species
single <- filter(merge,Species=="Acropora tenuis")
p <- ggplot(single,aes(x=hs.means)) +
  geom_point(aes(y=adj.He)) + 
  stat_smooth(aes(y=adj.He),method="lm") +
  theme_bw()
p
