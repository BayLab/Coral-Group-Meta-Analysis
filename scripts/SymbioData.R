####This script pulls out the symbiont data from the GeoSymbio database for a given set of species
###Rachael A. Bay
###Last edited 05.27.20

library(tidyverse)

species <- read.csv("data/Acropora_SpeciesList.csv")
sym.db <- read.csv("data/GeoSymbioData.csv")
names(sym.db)

####Filter symbiont database
acr.sym <- filter(sym.db,Host_Genus=="Acropora")
table(acr.sym$Host_Species)
sym.sub <- filter(acr.sym,Host_Sci_Name%in%species$X)
dim(sym.sub) # These are records for just our species

###Summarize
tab.type <- table(droplevels(sym.sub$Host_Sci_Name),droplevels(sym.sub$Type))
head(tab.type)
tab.type <- tab.type[,-31] #This is "N_A" but please double check!
head(tab.type)
tab.clade <- table(droplevels(sym.sub$Host_Sci_Name),droplevels(sym.sub$Clade)) #this is by "clade" instead

###How many different clades
sp.obs <- rowSums(tab.type) # How many total observations per species
sp.types <- apply(tab.type,1,function(x) length(which(x>0)))
sp.clades <- apply(tab.clade,1,function(x) length(which(x>0)))
hist(sp.types) # Wow - some have 14 types?!
plot(sp.types~sp.clades)

###Make and save an output
out <- data.frame(nobs=sp.obs,
                  nclades=sp.clades,
                  ntypes=sp.types,
                  as.data.frame.matrix(tab.clade))
index <- match(species$X,rownames(out))
out2 <- out[index,]
rownames(out2) <- species$X
write.csv(out2,"data/SymbioSummary.csv")
