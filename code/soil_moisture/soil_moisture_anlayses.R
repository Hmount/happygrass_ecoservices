#### Soil moisture analyses
#### test for significant difference in soil moisture between 
#### precipitation treatments (and seeding treatments?)

#### 9/29/25: some differences between soil moisture depend on 
#### month, but precipitation treatment, year, and their
#### interaction are all significant in ANOVA.

#### it is unclear which port goes to which seeding treatment.
#### if we have these data we can assess otherwise this is
#### more descriptive analysis than a function of CWMs.  

library(tidyverse)

## cleaned soil moisture data (all values in m3/m3 VWC)
smdat <- read.csv("code/soil_moisture/clean_soilmoisture.csv")

## ensure correct data format and variable names
colnames(smdat) <- c("Treatment", "Date","Moisture") #rename trt to treatment
smdat$Treatment <- as.factor(smdat$Treatment) #make factor
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

## find monthly average (for viewing and maybe plotting) (not sig. diff in models)
smdat.month <- smdat %>%
  group_by(Year, Month, Treatment) %>%
  summarise(moisturemean = mean(Moisture))
#find percent reduced per month per year
reduction.month <- smdat.month %>%
  pivot_wider(names_from = Treatment, values_from = moisturemean) %>%  # Spread cntl & drt into separate columns
  mutate(percent_reduction = ((cntl - drt) / cntl) * 100)  # Calculate percent reduction

## find annual average to use for analyses
smdat.yr <- smdat %>%
  group_by(Year, Treatment) %>%
  summarise(moistureyr = mean(Moisture))
#find percent reduced per month
reduction.yr <- smdat.yr %>%
  pivot_wider(names_from = Treatment, values_from = moistureyr) %>%  # Spread cntl & drt into separate columns
  mutate(percent_reduction = ((cntl - drt) / cntl) * 100)  # Calculate percent reduction

## mean reduction over course of experiment(currently only looks at
## 2021-2023).
sum(reduction.yr$percent_reduction)/3 # = 9.3% mean reduction, w/ 2024 is = 13.31%


### Test and model differences in soil moisture by year and precipitation treatment

### month does have a strong effect, but treatment, year, and interaction are all signfiggant with it
### so leaving this model out right now.
summary(lm(Moisture~Year*Month*Treatment, smdat))
anova(lm(Moisture~Year*Month*Treatment, smdat))
ggplot(smdat, aes(y=Moisture, x=Month, fill=Treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(y = "Soil Volumetric Water Content")+
  facet_wrap(~Year, nrow=1)

summary(lm(Moisture~Year*Treatment, smdat))
anova(lm(Moisture~Year*Treatment, smdat))
ggplot(smdat, aes(y=Moisture, x=Treatment, fill=Treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))#+
  facet_wrap(~Year)

