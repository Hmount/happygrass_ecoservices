#### wrangling trait data. 
#### Merge all trait data into a master dataframe for calculating CWM's
#### native species data all comes from Blumenthal 2020
#### invasive species data comes from a) additional measurements from blumenthal 2020
#### in nearby prairie, or b) measured from individuals directly on plot

## library
library(tidyverse)

## data
nativetraits <- read.csv("data/traits/mixedgrass.csv") #native species trait data
nativetraits <- nativetraits %>% filter(use == "1") #only species used in experiment

weeds1 <- read.csv("data/traits/HPGRS_weeds.csv") #borrowed invasive species trait data
weeds1 <- weeds1 %>% filter(Species == "BRTE" | #select only needed species
                              Species == "CHLE" |
                              Species == "SAIB" |
                              Species == "TRDU" |
                              Species == "BRIN" |
                              Species == "DESO" |
                              Species == "LIDA") 

weeds2 <-  read.csv("data/traits/HPG_WEEDS_complete.csv") #invasive trait data measured from site

## prep for merging all trait data
## for native data
nativetraitssub <- nativetraits %>% select(spcode,   #select only needed trait columns
                                           species,
                                           graminoid,
                                           sla,
                                           leafn,
                                           lop,
                                           ldmc,
                                           srl,
                                           rootdiam,
                                           rdmc,
                                           veg,
                                           c4)

## for borrowed invasive data
weeds1sub <- weeds1 %>% select(Species,    #select only needed trait columns
                               Species.Name,
                               Rep,
                               Functional.Group,
                               Life.History,
                               Trait,
                               Data) 
weeds1sub <- weeds1sub %>% pivot_wider(id_cols = c(Species, #reshape to select traits
                                              Species.Name,
                                              Rep,
                                              Functional.Group,
                                              Life.History), 
                                  names_from = Trait, 
                                  values_from = Data,
                                  values_fn = mean) #average duplicates
weeds1subav <- weeds1sub %>% group_by(Species,   #find species-level averages
                                      Species.Name,
                                      Functional.Group,
                                      Life.History) %>% 
  summarise(sla = mean(`Specific Leaf Area (SLA, cm2/g)`, na.rm=T),
            leafn = mean(`Leaf N (%)`, na.rm=T),
            lop = mean(`Leaf Osmotic Potential (MPa)`, na.rm=T),
            ldmc = mean(`Leaf Dry Matter Content (LDMC, g/g)`, na.rm=T),
            srl = mean(`Specific Root Length (SRL, m/g)`, na.rm=T),
            rootdiam = mean(`Root Avg Diameter (mm)`, na.rm=T),
            rdmc = mean(`Root Dry Matter Content (RDMC, g/g)`, na.rm=T))
weeds1subav <- weeds1subav %>% rename(spcode = Species, #rename grouping columns to match
                                      species = Species.Name) 

## for measured invasive data
weeds2av <- weeds2 %>% group_by(spcode,species) %>% #find species-level averages
  summarise(sla = mean(SLA..cm.g., na.rm=T),
            leafn = mean(leaf.N...., na.rm=T),
            #lop = mean(`Leaf Osmotic Potential (MPa)`, na.rm=T),
            ldmc = mean(LDMC..g.g., na.rm=T),
            srl = mean(SRL..m.g., na.rm=T),
            rootdiam = mean(root.diam..mm., na.rm=T))
            #rdmc = mean(`Root Dry Matter Content (RDMC, g/g)`, na.rm=T))

## Merge all trait data
mastertrait <- bind_rows(nativetraitssub,weeds1subav,weeds2av)
