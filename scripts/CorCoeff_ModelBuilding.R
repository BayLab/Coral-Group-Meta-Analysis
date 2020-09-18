library(dplyr)
library(ggplot2)

###Read in master data frame
master <- read.csv("data/MasterDataFrame.csv")
head(master)


##Aggregate by species/site
n.loci.threshold = 5 # Minimum number of microsats for a 'site'
persite <- master %>%
  group_by(loc,Species) %>%
  summarise(Number=mean(Number),
            Longitude=mean(Longitude),
            Latitude=mean(Latitude),
            Ocean=names(which.max(table(Ocean))),
            He=mean(He.all,na.rm=T),
            n.loci=n()) %>%
  filter(n.loci >= n.loci.threshold)
dim(persite)

##Look at 'well represented' species
persite$n.sites.species <- sapply(persite$Species,function(x) length(which(persite$Species==x))) #Add column that tells us how many sites for the species overall
nsites.threshold = 5 # Minimum number of sites/species
good.species <- persite %>%
  filter(n.sites.species >= nsites.threshold)
dim(good.species)
good.species.names <- unique(good.species$Species) # This is the number of species we are including
length(good.species.names)# This is the number of species we are including

##For each of our good species, calculate correlation (spearman) with env variables
env.vars <- c("He.all","Latitude","Longitude",
              "hs.freqs","pop",
              "BO2_tempmean_ss","BO2_ppmean_ss","BO_ph","BO_calcite","BO2_salinitymean_ss")
cor.coeffs <- matrix(data=NA,nrow=length(good.species.names),ncol=length(env.vars)-1)
for (i in 1:length(good.species.names)) {
  spec <- good.species.names[i]
  subframe <- filter(master,Species==spec)
  cor <- cor(subframe[,env.vars],use="na.or.complete")
  cor.coeffs[i,] <- cor[-1,1]
}
rownames(cor.coeffs) <- good.species.names
colnames(cor.coeffs) <- env.vars[-1]

par(mfrow=c(2,5),mar=c(3,3,3,1))
for (k in 1:ncol(cor.coeffs)) {
  hist(cor.coeffs[,k],main=colnames(cor.coeffs)[k])
  abline(v=0,col="red")
}

###Aggregate species traits
trait.ind <- c("PLD.MAX","Depth.upper","Range.size","Species.age.phylogeny")
perspecies <- aggregate(master[,trait.ind],by=list(master$Species),mean)
perspecies.order <- perspecies[match(rownames(cor.coeffs),perspecies$Group.1),]
species.frame <- data.frame(cor.coeffs,perspecies.order)
avg.He <- aggregate(master$He.all,by=list(master$Species),mean)
species.frame$avg.He <- avg.He$x[match(rownames(cor.coeffs),perspecies$Group.1)]


##########################################
### The work starts here!!
#########################################


###Build a model! - we need to do this for each geographic variable separately
##I'll give an example using 'hs.freqs'

##First, just look at the distribution of the coefficients
hist(species.frame$hs.freqs)
length(which(species.frame$hs.freqs>0))/length(species.frame$hs.freqs) 
# 61% of species have positive coefficients
t.test(species.frame$hs.freqs)
# mean is marginally different from 0

##Now, build a model with 'all' the trait variables
M <- lm(hs.freqs~PLD.MAX+Depth.upper+Range.size+Species.age.phylogeny,data=species.frame)
summary(M) #Nothing is significant
shapiro.test(M$residuals) # Testing for normality of residuals - we don't want this to be significant! If it is we'll need to transform some variables

##Is the model a better fit if we drop one of the traits?
drop1(M)
# A difference in AIC of >2 is usually considered 'better'
# In this case, we get the mose improvement by dropping 'Species.age.phylogeny'

##Make a new model with one variable dropped
M1 <- lm(hs.freqs~PLD.MAX+Depth.upper+Range.size,data=species.frame)
summary(M1)
shapiro.test(M1$residuals)
AIC(M,M1) # There is a >2 AIC difference

##Should we drop another?
drop1(M1)
M2 <- lm(hs.freqs~Depth.upper+Range.size,data=species.frame)
summary(M2)
shapiro.test(M2$residuals)
AIC(M1,M2) #uh oh, we have different numbers of observations, so this doesn't work anymore! Let's try the model on just complete rows
M2a <- lm(hs.freqs~Depth.upper+Range.size,data=species.frame[complete.cases(species.frame),])
AIC(M1,M2a) #not *quite* a 2 AIC difference

##So here, our final model is M2, but neither of those variables are significant
##NOTE! We don't have a ton of observations here:
nobs(M2)
