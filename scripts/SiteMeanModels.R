library(dplyr)
library(ggplot2)
library(geosphere)
library(rworldmap)

###Read in master data frame
master <- read.csv("data/MasterDataFrame.csv")
head(master)

##BRI
bri <- read.csv("data/SwainBRI.csv")
master$bri <- bri$Taxon.dependent.site.BRI.sBRIj..Eq.12...314.sites..[match(master$Species,bri$Taxon)]

###What if we just try the top 6 microsats?
#mics <- names(sort(table(master$Locus),decreasing=T))[1:3]
#filt <- master[master$Locus%in%mics,]
filt <- master

##Aggregate by species/site
n.loci.threshold = 5 # Minimum number of microsats for a 'site'
persite <- filt %>%
  group_by(loc,Species) %>%
  summarise(Number=mean(Number),
            Year=mean(Sample.year),
            Longitude=mean(Longitude),
            Latitude=mean(Latitude),
            Ocean=names(which.max(table(Ocean))),
            popdens=mean(pop,na.rm=T),
            He=mean(He.all,na.rm=T),
            He.adj=mean(adj.He,na.rm=T),
            n.loci=n(),
            hs=mean(hs.freqs,na.rm=T),
            temp=mean(BO2_tempmean_ss,na.rm=T),
            ph=mean(BO_ph,na.rm=T),
            salinity=mean(BO2_salinitymean_ss,na.rm=T),
            depth.upper=mean(Depth.upper,na.rm=T),
            depth.range=mean(Depth.upper-Depth.lower,na.rm=T),
            range.size=mean(Range.size,na.rm=T),
            corallite.max=mean(Corallite.width.maximum,na.rm=T),
            num.spec=mean(species,na.rm=T),
            bri=mean(bri,na.rm=T))%>%
  filter(n.loci >= n.loci.threshold)
dim(persite)
persite$Number <- as.factor(persite$Number)


###Random forest
library(randomForest)
nona <- na.omit(persite)[,2:ncol(persite)]
rf <- randomForest(He~.,data=nona[,c(1:8,11:19)])
rf
varImpPlot(rf)

###Just exploratory plots for top variables
pac <- subset(persite,Ocean=="Pacific")
ten <- subset(persite,Species=="Acropora tenuis")
mil <- subset(persite,Species=="Acropora millepora")

plot(persite$He.adj~persite$Year,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$bri,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$depth.range,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$Latitude,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$Longitude,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$range.size,col=as.factor(persite$Ocean))
plot(persite$He.adj~persite$corallite.max,col=as.factor(persite$Ocean))
plot(mil$He,mil$num.spec)
plot(ten$He,ten$num.spec)

#Latitude with polynomial
p <- ggplot(persite,aes(x=Latitude,y=He.adj)) +
  geom_point(aes(y=He.adj,color=Ocean)) + # Need to test if quadratic (u-shaped) fits this better
  theme_bw()
p


###linear models for top geographic variables
library(lmerTest)
persite$lat2 <- persite$Latitude^2
M.all <- lmer(He.adj~Ocean+ph+temp+(1|Species)+(1|Number),data=persite)
M.pac <- lmer(He.adj~ph+temp+(1|Species)+(1|Number),data=pac)
M.ten <- lmer(He.adj~ph+temp+(1|Number),data=ten)
M.mil <- lmer(He.adj~ph+temp+(1|Number),data=mil)
M.quad <- lmer(He.adj~Latitude+(1|Species)+(1|Number),data=persite)
M.year <- lmer(He.adj~Year+(1|Species)+(1|Number),data=persite)

###linear models for top variables - Organismal
no.out <- persite[persite$depth.upper<15,]
no.out.pac <- subset(no.out,Ocean=="Pacific")
M.all <- lmer(He.adj~depth.range+(1|Species)+(1|Number),data=persite)
M.pac <- lmer(He.adj~corallite.max+(1|Species)+(1|Number),data=pac)


##Look at 'well represented' species
persite$n.sites.species <- sapply(persite$Species,function(x) length(which(persite$Species==x))) #Add column that tells us how many sites for the species overall
nsites.threshold = 5 # Minimum number of sites/species
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

###Let's look at 'just' the GBR
mymap <- getMap(resolution = "low")
GBR=subset(good.species,Latitude<(-10) & Longitude>130 & Longitude<155)
plot(GBR$Longitude,GBR$Latitude,col=as.factor(GBR$Species))
plot(mymap,lwd=1.5,add=T)
plot(GBR$He.adj~GBR$Latitude,col=as.factor(persite$Species))
summary(lm(GBR$He.adj~GBR$Latitude+GBR$Species))

p <- ggplot(GBR,aes(y=He.adj,x=Latitude,color=Species)) +
  geom_point(aes(y=He.adj)) + 
  stat_smooth(aes(y=He.adj),method="loess") + 
  theme_bw()
p

