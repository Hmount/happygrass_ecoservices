#### Soil moisture analyses
#### test for significant difference in soil moisture between 
#### precipitation treatments and seeding treatments?

#### 9/29/25: some differences between soil moisture depend on 
#### month, but precipitation treatment, year, and their
#### interaction are all significant in ANOVA.

#### 11/17/25: analysis to test seeding treatments added. CWM's for all
#### years has not been calculated yet, only assessing 2021-2023** 
#### **need to add 2024 data


## library loading
library(tidyverse)

## cleaned soil moisture data (all values in m3/m3 VWC)
smdat <- read.csv("code/soil_moisture/clean_soilmoisture_byalltrt.csv")

## ensure correct data format and variable names
colnames(smdat) <- c("Block","seedtrt", "droughttrt", "Date","Moisture") #rename trt to treatment
smdat$droughttrt <- as.factor(smdat$droughttrt) #make factor
smdat$seedtrt <- as.factor(smdat$seedtrt) #make factor
smdat <- smdat %>% #seperate date column
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", remove = T)
smdat$Year <- as.factor(smdat$Year) #make factor
smdat$Month <- as.factor(smdat$Month) #make factor
smdat$Day <- as.factor(smdat$Day) #make factor
## filter for only growing season (May through August)
## probes are unreliable in winter
smdat <- smdat %>%
  filter(Month %in% c("05", "06", "07", "08")) %>%
  mutate(Month = droplevels(Month))  # Removes unused levels

smdat <- smdat %>%
  mutate(seedtrt = relevel(seedtrt, ref = "rand"))

smdat <- smdat %>%filter(seedtrt != "c"& seedtrt != "ir")
# 
# ## mean reduction over course of experiment.
# sum(reduction.yr$percent_reduction)/3 # = 9.3% mean reduction, w/ 2024 is = 13.31%
# # with cheatgrass strip/ bare/ invaded removed = 18.42% mean reduction

## mean reduction over course of experiment by seeding treatment
smdat.yr.trt <- smdat %>%
  #filter(seedtrt != "c") %>% #remove cheatgrass strip?
  group_by(Year, droughttrt, seedtrt) %>%
  summarise(moistureyr = mean(Moisture))
reduction.yr.trt <- smdat.yr.trt %>%
  pivot_wider(id_cols = c(Year,seedtrt), names_from = droughttrt, values_from = moistureyr) %>%  # Spread cntl & drt into separate columns
  mutate(percent_reduction = ((cntl - drt) / cntl) * 100)  # Calculate percent reduction

ploybyyear <- reduction.yr.trt %>% group_by(seedtrt, Year) %>% summarise(mean=percent_reduction)
ggplot(ploybyyear, aes(x=seedtrt, y=mean))+
  geom_boxplot()+
  facet_wrap(~Year, nrow=1)
#positive numbers are reductions caused by drought treatment 
#negative numbers are when drought treatment had greater moisture than ambient
#tester <- reduction.yr.trt %>% filter(Year != "2024")
reducdataframe <- reduction.yr.trt %>% group_by(seedtrt) %>% summarise(mean=percent_reduction)
ggplot(reducdataframe, aes(x=seedtrt, y=mean))+
  geom_boxplot()+
  labs(y="Percent reduction", x="Seeding treatment")+
  annotate("text",x="rand", y=25, label="13.2%")+
  annotate("text",x="dt", y=25, label="9.71%")+
  annotate("text",x="fd", y=25, label="10.8%")+
  theme_bw()
reducdataframe %>% summarise(mean=mean(mean))
summary(lm(mean~seedtrt, reducdataframe)) #not significant (too few replicates?)
anova(lm(mean~seedtrt, reducdataframe))


### Test and model differences in soil moisture by year and precipitation treatment

# ### month does have a strong effect, but 
# ### we are only interested in the effect of year (below)
# summary(lm(Moisture~Year*Month*Treatment, smdat))
# anova(lm(Moisture~Year*Month*Treatment, smdat))
# #figure 
# ggplot(smdat, aes(y=Moisture, x=Month, fill=Treatment))+
#   geom_boxplot()+
#   scale_fill_manual(values=c("skyblue","tomato2"), labels = c("ambient", "reduction"))+
#   labs(y = "Soil Volumetric Water Content", fill= "Precipitation 
# treatment")+
#   facet_wrap(~Year, nrow=1)+
#   theme_bw()


### Test and model differences in soil moisture by year and precipitation treatment
### and seeding treatment
summary(lm(Moisture~Year*Month*seedtrt*droughttrt, smdat))
anova(lm(Moisture~Year*Month*seedtrt*droughttrt, smdat))
#figure in replort:
ggplot(smdat, aes(y=Moisture, x=seedtrt, fill=droughttrt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"), labels = c("ambient", "reduction"))+
  labs(y = "Soil Volumetric Water Content", fill= "Precipitation 
treatment")+
  facet_grid(Month~Year)+
  theme_bw()

#dry years have stronger effect and drt only matters in dry years
summary(mod <- lmer(Moisture~Year*seedtrt*droughttrt+(1|Block), smdat))
anova(lmer(Moisture~Year*seedtrt*droughttrt+(1|Block), smdat))
summary(lm(Moisture~droughttrt, smdat))
ggplot(smdat, aes(y=Moisture, x=seedtrt, fill=droughttrt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(y = "Soil Volumetric Water Content", x = "Seeding Treatment", fill= "Precipitation 
treatment")+
#   labs(x="Seeding treatment", y="Soil Moisture", fill="Precipitation 
# treatment")+
  facet_wrap(~Year, nrow = 1)+
  #facet_grid(Year~Month)
  theme_bw()


## ~ seeding treatment
#create letters for plotting:
library(emmeans)
# Step 1: Get the emmeans for the interaction of trt, drought, and year
emm_trt <- emmeans(mod, ~ seedtrt * droughttrt | Year)
## # Step 2: Obtain pairwise contrasts for the interaction
## contrast_trt <- contrast(emm_trt, method = "pairwise")
# Step 2: Generate the compact letter display using multcomp::cld
cld_res <- multcomp::cld(emm_trt, by = "Year", adjust = "tukey", Letters=letters)
# Step 3: Convert the results to a data frame
letters_df <- as.data.frame(cld_res)
# Step 4: Create a temporary data frame with the desired y-position for plotting
dttemp2 <- smdat %>%
  group_by(Year, seedtrt, droughttrt) %>%
  summarise(yposition = quantile(Moisture,.99, na.rm = T), .groups = 'drop')
# Step 5: Merge the letter results with the y-position data
dttemp2 <- merge(letters_df, dttemp2, by = c("droughttrt", "seedtrt","Year"))
# Merge with the original data to get the final dataset
dttemp3 <- merge(smdat, dttemp2, by = c("droughttrt", "seedtrt", "Year"), all = TRUE)

#plot:
ggplot(dttemp3, aes(y=Moisture, x=seedtrt, fill=droughttrt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x="Seeding treatment", y="Soil Moisture", fill="Precipitation 
treatment")+
  ylim(0,0.45)+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .9), 
            vjust = -1.75,
            hjust = .5,
            size=3)+
  facet_wrap(~Year, nrow = 1)+
  theme_bw()


#### CWM's and FD and soil moisture
### What about the relationship between CWM and services?
smdat.yr.trt.b <- smdat %>%
  #filter(seedtrt != "c") %>% #remove cheatgrass strip?
  group_by(Year, droughttrt, seedtrt, Block) %>%
  summarise(moistureyr = mean(Moisture))
comms21 <- read.csv("data/communities/validCWM21.csv")
comms21$year <- "2021"
comms22 <- read.csv("data/communities/validCWM22.csv")
comms22$year <- "2022"
comms <- bind_rows(comms21,comms22)
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
comms <- bind_rows(comms,comms23)
#subset comms to just have CWM's for 10 best blocks with soil moisture data
best10 <- c(20, 22, 27, 30, 33, 42, 49, 50, 51, 58)
subcomms <- comms %>% filter(block %in% best10)
subcomms <- subcomms %>% mutate(droughttrt = ifelse(drought=="0","cntl","drt"))
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
smcomms.cwm <- merge(smdat.yr.trt.b,subcomms, 
                 by.x=c("Block","seedtrt","droughttrt","Year"), 
                 by.y = c("block","trt","droughttrt","year"),
                 all.y=T)
smcomms.cwm <- smcomms.cwm %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable


summary(lmer(moistureyr ~ droughttrt*ldmc+(1|plot), data=smcomms.cwm)) 
smldmcplot <- ggplot(smcomms.cwm, aes(x=ldmc, y=moistureyr, col=droughttrt))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="CWM LDMC",y="Soil moisture", col="Precipitation
treatment")+
 # facet_wrap(~Year)
  theme_bw()

### What about the relationship between CWM and services?
comms21.fd <- read.csv("data/communities/FD21.csv")
comms21.fd$year <- "2021"
comms22.fd <- read.csv("data/communities/FD22.csv")
comms22.fd$year <- "2022"
comms.fd <- bind_rows(comms21.fd,comms22.fd)
comms23.fd <- read.csv("data/communities/FD23.csv")
comms23.fd$year <- "2023"
comms.fd <- bind_rows(comms.fd,comms23.fd)
#subset comms to just have CWM's for 10 best blocks with soil moisture data
best10 <- c(20, 22, 27, 30, 33, 42, 49, 50, 51, 58)
subcomms.fd <- comms.fd %>% filter(block %in% best10)
subcomms.fd$droughttrt <- subcomms$droughttrt
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
smcomms.fd <- merge(smdat.yr.trt.b,subcomms.fd, 
                     by.x=c("Block","seedtrt","droughttrt","Year"), 
                     by.y = c("block","trt","droughttrt","year"),
                     all.y=T)
smcomms.fd <- smcomms.fd %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable

summary(lmer(moistureyr ~ droughttrt*rootdiam+(1|plot), data=smcomms.fd)) 
smrdfdplot <- ggplot(smcomms.fd, aes(x=rootdiam, y=moistureyr, col=droughttrt))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="FD root diameter",y="Soil moisture", col="Precipitation
treatment")+
  #facet_wrap(~Year)
  theme_bw()

summary(lmer(moistureyr ~ droughttrt*full+(1|plot), data=smcomms.fd)) 
smfullplot <- ggplot(smcomms.fd, aes(x=full, y=moistureyr, col=droughttrt))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="FD",y="Soil moisture", col="Precipitation
treatment")+
  theme_bw()

library(ggpubr)
ggarrange(smldmcplot, smrdfdplot, smfullplot, common.legend = T, nrow=1)
