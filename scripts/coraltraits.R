library(ggplot2)
library(lme4)
library(plotly)
library(ggridges)
library(dplyr)
library(tidyr)

# set wd
setwd("/Users/brookebenson/Desktop/Dropbox/Coral Meta-Analysis/Coral-Group-Meta-Analysis/data")

# read in the datasets
t <- read.csv('trait-database.csv')
g <- read.csv('GeneticData_Acropora_cleaned.csv')

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
           species == 'micropthalma' |
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

# let's begin by exploring the traits at the global level
# create a subset of data that only includes global estimates
global <- t %>%
  filter(location_name == 'Global estimate')

# range size (km^2)
range <- global %>%
  filter(trait_name == 'Range size')
range$value <- as.numeric(paste(range$value))
hist(range$value)

# corallite width minimum
cwmin <- global %>%
  filter(trait_name == 'Corallite width minimum')
cwmin$value <- as.numeric(paste(cwmin$value))
hist(cwmin$value)

# corallite width maximum
cwmax <- global %>%
  filter(trait_name == 'Corallite width maximum')
cwmax$value <- as.numeric(paste(cwmax$value))
hist(cwmax$value)
    # how do max/min corallite diameter correlate?
    cw <- merge(cwmin, cwmax, by = 'species')
    plot(cw$value.x~cw$value.y)

# colony maximum diameter (standard unit is cm)
colmaxdiam <- global %>%
  filter(trait_name == 'Colony maximum diameter')
colmaxdiam$value <- as.numeric(paste(colmaxdiam$value))
# divide by 100 to change to m
colmaxdiam$value <- (colmaxdiam$value/100)
hist(colmaxdiam$value)







#####

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




