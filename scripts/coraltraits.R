library(ggplot2)
library(lme4)
library(plotly)
library(ggridges)
library(dplyr)
library(tidyr)

# set wd
setwd("/Users/brookebenson/Desktop/Dropbox/Coral-Group-Meta-Analysis/")

# read in the datasets
t <- read.csv('data/trait-database.csv')
g <- read.csv('data/GeneticData_Acropora_cleaned.csv')

# make separate columns for genus and species to isolate species 
 t <- t %>% separate(specie_name, c('genus','species'))

# filter the trait data to include only the species for which we have genetic data
t <- t %>%
  filter(genus == "Acropora") %>%
  filter(species == 'austera' |
           species == 'cervicornis' |
           species == 'cytherea' |
           species == 'digitifera' |
           species == 'florida' |
           species == 'horrida' | 
           species == 'hyacinthus' |
           species == 'jacquelineae' |
           species == 'kimbeensis' |
           species == 'kirstyae' | 
           species == 'microphthalma' |
           species == 'millepora' |
           species == 'muricata' |
           species == 'palmata' |
           species == 'papillare' |
           species == 'pichoni' |
           species == 'pulchra' |
           species == 'rongelapensis' |
           species == 'sarmentosa' |
           species == 'solitaryensis' |
           species == 'sp. 1' |
           species == 'spathulata' |
           species == 'spicifera' |
           species == 'tenuis' |
           species == 'tortuosa' |
           species == 'valida' |
           species == 'walindii' |
           species == 'yongei')

# put the data in wide format
t.wide<- spread(t,trait_name,value)
# data frame was being weird, write csv and read back in
write.csv(t.wide,'data/t.wide.csv')
t <- read.csv("data/t.wide.csv")

# plot every possible trait of interest to see how many species there are data for (just swap out x axis)
ggplot(t, aes(x=Western.most.range.edge, y=species, group = species))+
  theme_bw()+
  geom_point(aes(color = species))+
  scale_y_discrete(limits = rev(levels(t$species)))+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

# what about global data?
# create a subset of data that only includes global estimates
global <- t %>%
  filter(location_name == 'Global estimate')

# out of curiosity...
# what are the geographic dimensions of our trait database?
min(t$latitude, na.rm = T) # -32.02379
max(t$latitude, na.rm = T) # 40.79339
min(t$longitude, na.rm = T) # -169.5093
max(t$longitude, na.rm = T) # 162.1476

# what are the geographic dimensions of our genetic database?
min(g$Latitude) # -31.55
max(g$Latitude) # 34.3601
min(g$Longitude) # -171.14
max(g$Longitude) # 171.68




