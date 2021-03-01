library(dplyr)
library(ggplot2)
library(rworldmap)
library(beeswarm)

##Read in Excel spreadsheet
d <- read.csv("~/Documents/CoralRG/Meta-Analysis/data/Porites_microsat_data.csv")
dim(d)
head(d)
##Give each location a unique identifier
d$loc <- paste(d$Number,d$Latitude,d$Longitude,sep="_")

##Add "Ocean" factor - do we want to break it down even further by region?
d$Ocean <- "Pacific"
d$Ocean[d$Longitude>-100 & d$Longitude<0] <- "Caribbean"
d$Ocean[d$Longitude>0 & d$Longitude<100] <- "Indian"

##Do some filtering
N.threshold = 10 # Sample size threshold
filt <- d %>% 
  filter(Sample.Size >= N.threshold) %>%
  filter(He != 0 | is.na(He)) # Remove non-variant loci
dim(filt)

##For studies where only mean was reported, sample based on mean and sd
##For now, I'm just doing heterozygosity
sub <- filt[!is.na(filt$He),]
mean.He <- aggregate(sub$He,list(sub$Number,sub$Species),mean)
sd.He <- aggregate(sub$He,list(sub$Number,sub$Species),sd)
mean.sd <- sd(mean.He$x) # Use this sd for error when it is not available
filt$He.all <- c()
for (i in 1:nrow(filt)) {
  if (!is.na(filt$He[i])){ # If He is available for individual locus, use that
    filt$He.all[i] <- filt$He[i]
  }
  else if (!is.na(filt$He.mean[i]) & !is.na(filt$He.sd[i])) { #If mean and sd are available
    filt$He.all[i] <- rnorm(1,mean=filt$He.mean[i],sd=mean.sd) # Decided just to use overall mean because some reported sd are really wonky
  }
  else if (!is.na(filt$He.mean[i]) & is.na(filt$He.sd[i])) { # If sd is not available
    filt$He.all[i] <- rnorm(1,mean=filt$He.mean[i],sd=mean.sd)
  }
}
filt$He.all[filt$He.all>1] <- 1 # If we happen to get a >1 value, adjust to 1

dim(filt)
table(filt$Species)

##Aggregate by species/site
n.loci.threshold = 5 # Minimum number of microsats for a 'site'
persite <- filt %>%
  group_by(loc,Species) %>%
  summarise(Number=mean(Number),
            Longitude=mean(Longitude),
            Latitude=mean(Latitude),
            Ocean=names(which.max(table(Ocean))),
            He=mean(He.all,na.rm=T),
            n.loci=n()) %>%
  filter(n.loci >= n.loci.threshold)
dim(persite)
persite$Number <- as.factor(persite$Number)

#Latitude with polynomial
p <- ggplot(persite,aes(x=Latitude,y=He)) +
  geom_point(aes(y=He,color=Number)) + 
  stat_smooth(aes(y=He),method="lm",formula=y~poly(x,2)) + # Need to test if quadratic (u-shaped) fits this better
  theme_bw()
p

library(lmerTest)
persite$lat2 <- persite$Latitude^2
M.quad <- lmer(He~lat2+Latitude+(1|Species),data=pac)

##Map locations for all species
plot(persite$Longitude,persite$Latitude,pch=19,col=persite$Number)
mymap <- getMap(resolution = "low")
plot(mymap,add=T,lwd=1.5)

##Look at 'well represented' species
persite$n.sites.species <- sapply(persite$Species,function(x) length(which(persite$Species==x))) #Add column that tells us how many sites for the species overall
nsites.threshold = 1 # Minimum number of sites/species
good.species <- persite %>%
  filter(n.sites.species >= nsites.threshold)
dim(good.species)
good.species.names <- unique(good.species$Species) # This is the number of species we are including
length(good.species.names)# This is the number of species we are including


###Some descriptive plots
##Just plot distribution of He for well-represented species
p <- ggplot(good.species,aes(y=He,x=Species,color=Species)) +
  geom_boxplot() +
  #  geom_dotplot(binaxis='y',stackdir="center",dotsize=0.5) +
  geom_jitter(shape=16,position=position_jitter(0.2)) +
  theme_bw() 
p #Why is palmata so high?
