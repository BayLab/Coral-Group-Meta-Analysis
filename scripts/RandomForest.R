library(dplyr)
library(ggplot2)
library(randomForest)

###Read in master data frame
master <- read.csv("data/MasterDataFrame.csv")
head(master)
names(master)

### Let's see how much missing data there is for each variable
vars <- names(master)[26:ncol(master)]
nas <- apply(master[,vars],2,function(x) sum(is.na(x)))
nas


###
sub.vars <- c("Species","Latitude","Longitude","Number","Primer.note","Cross.species","He.all",
              "hs.freqs","pop",
              "BO2_tempmean_ss","BO2_ppmean_ss","BO_ph","BO_calcite","BO2_salinitymean_ss",
              "PLD.MAX","PLD.MIN","PLD.PEAK","Depth.lower","Depth.upper","Range.size",
              "Range.size.se","Species.age.phylogeny")

###First we'll try to put everything into a single data frame
sub.frame <- master[,sub.vars]
nona <- na.omit(sub.frame)
dim(nona)
rf <- randomForest(He.all~.,data=nona)
rf # We can see % Variance explained here
varImpPlot(rf)

##"Paired down" version
sub.vars <- c("Species","Latitude","Longitude","Number","He.all",
              "hs.freqs","pop",
              "BO2_tempmean_ss","BO_ph","BO2_salinitymean_ss",
              "PLD.MAX","Depth.upper","Range.size",
              "Species.age.phylogeny")
subframe <- na.omit(master[,sub.vars])
rf <- randomForest(He.all~.,data=subframe)
rf
varImpPlot(rf)

###Now just geographic variables
env.vars <- c("Species","Latitude","Longitude","Number","Primer.note","Cross.species","He.all",
              "hs.freqs","pop",
              "BO2_tempmean_ss","BO2_ppmean_ss","BO_ph","BO_calcite","BO2_salinitymean_ss")
env.frame <- na.omit(master[,env.vars])
env.rf <- randomForest(He.all~.,data=env.frame)
env.rf
varImpPlot(env.rf)

#What happens if we remove latitude and longitude?
env.rf.nogeo <- randomForest(He.all~.,data=env.frame[,-2:-3])
env.rf.nogeo
varImpPlot(env.rf.nogeo)


###Now just organismal variables
org.vars <- c("Species","Latitude","Longitude","Number","Primer.note","Cross.species","He.all",
              "PLD.MAX","PLD.MIN","PLD.PEAK","Depth.lower","Depth.upper","Range.size",
              "Range.size.se","Species.age.phylogeny")
org.frame <- na.omit(master[,org.vars])
org.rf <- randomForest(He.all~.,data=org.frame)
org.rf
varImpPlot(org.rf)

#Organismal variable without Species
org.rf.nospec <- randomForest(He.all~.,data=org.frame[,-1])
org.rf.nospec
varImpPlot(org.rf.nospec)

##If you want to look at just one variable
p <- ggplot(master,aes(x=BO_ph)) +
  geom_point(aes(y=adj.He)) + 
  stat_smooth(aes(y=adj.He),method="lm") +
  theme_bw()
p

##If you want to look in just one species
single <- filter(master,Species=="Acropora tenuis")
p <- ggplot(single,aes(x=BO_ph)) +
  geom_point(aes(y=adj.He)) + 
  stat_smooth(aes(y=adj.He),method="lm") +
  theme_bw()
p
