library(ggplot2)
library(lme4)
library(plotly)
library(ggridges)
library(dplyr)
library(tidyr)

# set wd
setwd("/Users/brookebenson/Desktop/Dropbox/Coral-Group-Meta-Analysis/")

# read in the datasets
t <- read.csv('data/Trait_database.csv')
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
plot <- ggplot(t, aes(x=Range.size, y=species, group = species, text = paste("location_name",location_name)))+
  theme_bw()+
  geom_point(aes(color = species))+
  scale_y_discrete(limits = rev(levels(t$species)))+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	


# subset of trait data that we want to look at (for now)
# obtained by removing all the stuff we DON'T want (negative sign)
t <- t %>% 
  select(-Abundance.GBR,-Abundance.world,-Axial.corallite.part,-Bleaching.event,-Chlorophyll.a, -Coloniality,-Colony.area,-Colony.shape.factor,-Dark.respiration,Depth.transplanted.from, -Distance.from.crest, -Distance.from.tip, -Flow.rate, -Genus.fossil.age,-Genus.fossil.stage,-Gross.photosynthesis, -Irradiance, -Life.history.strategy, -Lipid.content,-Mitotic.index, -Mode.of.larval.development, -Nitrogen.concentration, -Phosphorus.concentration, -Polyp.fecundity, -Polyps.per.area, -Propagule.size.on.release, -Protein.biomass, -Ratio.of.female.to.male.gonads, -RNA.DNA.ratio, -Sample.identifier, -Season, -Sexual.system, -Size.at.maturity, Skeletal.micro.density, -Substrate.attachment, -Symbiodinium.sp..in.propagules, -Tissue.thickness, -Total.biomass, -UV.radiation.treatment, -Water.depth, -Water.temperature, -Wave.exposure, -Year, -Zooxanthellate) %>%
  filter(location_name == 'Global estimate')


# conversely, this is how you would select the ones you DO want
#t <- t %>%
#  select(Calcification.rate, Colony.maximum.diameter, Corallite.width.maximum,Corallite.width.minimum, Depth.lower, Depth.upper, Eastern.most.range.edge, Geographical.region, Growth.form.typical, Growth.form.Veron, Growth.form.Wallace, Growth.outline.type, Growth.rate, Habitat.type, Indo.Pacific.faunal.province, IUCN.Red.List.category, Northern.most.range.edge, Oocyte.size.at.maturity, Range.size,	Skeletal.density, Southern.most.range.edge, Species.age.phylogeny, Symbiodinium.clade, Symbiodinium.subclade, Water.clarity.preference, Wave.exposure.preference, Western.most.range.edge)

# okay, here's how I extracted the global estimates value for the Global.estimates (Global.estimates.csv) tab of the Trait Data workbook
# make a new object, pulling out only the relevant info
  # we want to know exactly what obervation this is in the trait database so we can go back to where we found this data if needed (obervation_id)
  # we also want the variable/trait (here, Western.most.range.edge)
  # location name should be "Global estimate" for everything we pull out with this because we already filtered for global estimate earlier on, but this is a good sanity check
temp <- t %>%        
  select(observation_id,species, Western.most.range.edge, location_name) %>%
# filter out anything in the database that has no data for the trait of interest
  filter(Western.most.range.edge != 'NA')

# this will write (output and save) a spreadsheet of the object we created above
# I did this for every trait of interest, so I now have a subfolder (GlobalTraitExports) of these stored in our data folder 
write.csv(temp, 'data/GlobalTraitExports/Western.most.range.edge.csv')

# trait data global estimates 
data <- read.csv('data/Global.estimates.csv')
head(data)







#####
# Miscellaenous code
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







