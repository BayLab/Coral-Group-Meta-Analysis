library(dplyr)
library(ggplot2)
library(rworldmap)
library(beeswarm)

##Read in Excel spreadsheet
d <- read.csv("~/Documents/CoralRG/Meta-Analysis/data/GeneticData_Acropora_cleaned.csv")
dim(d)

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

##Plot all "He.all" points against latitude
p <- ggplot(filt,aes(x=Latitude)) +
  geom_point(aes(y=He.all)) + 
  stat_smooth(aes(y=He.all),method="lm",formula=y~poly(x,2)) +
  theme_bw()
p # Doesn't look like much :P

###Try to see what the important variables are
##I'm using random forest for now - not sure what the 'best' method is
library(randomForest)
imp.vars <- c("He.all","Species","Ocean",
              "Latitude","Longitude",
              "Number","Cross.species","Primer.note")
rf <- randomForest(He.all~.,data=filt[,imp.vars])
varImpPlot(rf)

ind <- sample(1:nrow(filt),round(nrow(filt)*0.75),replace=F) #Training set is 75% of data
train <- filt[ind,imp.vars]
val <- filt[-ind,imp.vars]
rf.train <- randomForest(He.all~.,data=train,importance=T,ntree=200)
rf.train # Summary of model
p <- predict(rf.train,val)
plot(p~val$He.all,ylab="Predicted He",xlab="Observed")
abline(lm(p~val$He.all))
summary(lm(p~val$He.all)) # Not too bad! On the official run we'll need to do this *many* times

##Other variables can be added to the random forest framework above.
##This will be especially useful for spatial variables






#####################################
### Aggregating by species/site - this is pretty exploratory/descriptive so far
####################################


##Aggregate to mean He per site
n.loci.threshold = 5 # Minimum number of microsats for a 'site'
persite <- filt %>%
          group_by(loc,Species) %>%
          summarise(Number=mean(Number),
                    Longitude=mean(Longitude),
                    Latitude=mean(Latitude),
                    Ocean=names(which.max(table(Ocean))),
                    He=mean(He.all,na.rm=T),
                    Ho=mean(Ho,na.rm=T),
                    He.mean=mean(He.mean,na.rm=T),
                    Ho.mean=mean(Ho.mean,na.rm=T),
                    n.loci=n()) %>%
        filter(n.loci >= n.loci.threshold)
persite$He.all <- rowMeans(persite[,c("He","He.mean")],na.rm=T) #This contains He reported both per locus and as a mean
dim(persite)

# read in the global trait estimates
globaltraits <- read.csv('data/Global.estimates.csv')
# add a column for full species name that will match the corresponding column in the genetic database
globaltraits$Species<- paste("Acropora",data$species, sep = " ")
# merge the two spreadsheets by Species
traitmerge <- merge(persite,globaltraits, by = 'Species')
head(traitmerge)

##Map locations for all species
plot(persite$Longitude,persite$Latitude,pch=19,col="red")
mymap <- getMap(resolution = "low")
plot(mymap,add=T,lwd=1.5)

p <- ggplot(persite,aes(x=Latitude,color=Ocean)) +
  geom_point(aes(y=He.all)) + 
  stat_smooth(aes(y=He.all),method="lm",formula=y~poly(x,2)) + # Need to test if quadratic (u-shaped) fits this better
  theme_bw()
p

######## Brooke
plot <- ggplot(traitmerge, aes(x=Colony.maximum.diameter, y=He.all, text = paste("loc",loc)))+
  theme_bw()+
  geom_point(aes(color = Species))+
#  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Corallite.width.minimum, y=He.all))+
  theme_bw()+
  geom_point(aes(color = Species))+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Corallite.width.maximum, y=He.all))+
  theme_bw()+
  geom_point(aes(color = Species))+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Depth.lower, y=He.all))+
  theme_bw()+
  geom_point(aes(color = Species))+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Depth.upper, y=He.all))+
  theme_bw()+
  geom_point(aes(color = Species))+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Range.size, y=He.all))+
  theme_bw()+
  geom_point(aes(color = Species))+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Species.age.phylogeny, y=He.all,text = paste("loc",loc)))+
  theme_bw()+
  geom_point(aes(color = Species))+
#  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

plot <- ggplot(traitmerge, aes(x=Growth.form.typical, y=He.all,text = paste("loc",loc)))+
  theme_bw()+
  geom_boxplot(aes())+
  #  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	




##Look at 'well represented' species
persite$n.sites.species <- sapply(persite$Species,function(x) length(which(persite$Species==x))) #Add column that tells us how many sites for the species overall
nsites.threshold = 20 # Minimum number of sites/species
good.species <- persite %>%
                filter(n.sites.species > nsites.threshold)

plot(good.species$Longitude,good.species$Latitude,pch=19,col=as.factor(good.species$Species))
plot(mymap,add=T,lwd=1.5)

##Mean He vs. latitude for species with at least 20 sites
p <- ggplot(good.species,aes(y=He.all,x=Latitude,color=Ocean)) +
  geom_jitter(size=1) + theme_bw() + 
  geom_smooth(method="lm",formula=y~poly(x,2))
p

##Beeswarm showing He for well represented species
p <- ggplot(good.species,aes(y=He.all,x=Species,color=Species)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y',stackdir="center",fill=NA) +
  theme_bw() 
p #Why is palmata so high?


##Single Species
single <- filter(good.species,Species=="Acropora tenuis") #Can change to explore any species

#Map locations for single species
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(single$Longitude,single$Latitude,col="red",pch=19)
plot(mymap,add=T,lwd=1.5)

#Plot mean He vs. latitude
p <- ggplot(single,aes(x=Latitude,color=Ocean)) +
  geom_point(aes(y=He.all)) + 
  stat_smooth(aes(y=He.all),method="lm") +
  theme_bw()
p


