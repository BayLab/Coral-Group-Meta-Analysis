#################
#LARVAL DURATION#
#################

library(ggplot2)
library(lme4)
library(plotly)
library(ggridges)
library(dplyr)
library(tidyr)

# read in the datasets
PLD <- read.csv('data/PLD_Acroporates.csv')
GEN <- read.csv('data/Heterozygosity_fixed_07.28.20.csv')
trait <- read.csv('data/Global.estimates.csv')

# look for basic information on PLD
PLD <- PLD %>%
  select(Species,PLD.MAX,PLD.MIN,PLD.PEAK)
tibble(PLD)
mean(PLD$PLD.MAX)
mean(PLD$PLD.PEAK)
mean(PLD$PLD.MIN)

plot <- ggplot(PLD, aes(x=PLD.MAX, y=Species,group = Species, text = paste("Species",Species)))+
  theme_bw()+
  geom_point(aes(color = Species))+
#  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

# clean trait data for merging, i.e. add a column for full species name that will match the corresponding column in the genetic database
trait$Species<- paste("Acropora",data$species, sep = " ")

# merge the datasets
PLDtraitmerge <- merge(PLD,trait,by.x=c("Species"),by.y=c("Species"))
tibble(PLDtraitmerge)

PLDGENmerge <- merge(PLD,GEN,by.x=c("Species"),by.y=c("Species"))
tibble(PLDGENmerge)

# preliminary analysis of PLD vs GEN (PLDGENmerge)
plot <- ggplot(PLDGENmerge, aes(x=PLD.MIN, y=He.all, group = Species, text = paste("loc",loc)))+
  theme_bw()+
  geom_point(aes(color = Species))+
#  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	

# preliminary analysis of PLD vs traits (PLDtraitmerge)
plot <- ggplot(PLDtraitmerge, aes(x=Range.size, y=PLD.MAX))+
  theme_bw()+
  geom_point(na.rm = T)+
  geom_smooth()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
ggplotly(subplot(list(plot),nrows=1,titleY=F) %>% layout(showlegend=T))	



