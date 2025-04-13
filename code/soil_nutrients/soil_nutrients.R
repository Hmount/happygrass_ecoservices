#### Nutrient probes wrangling and analysis
#### Probes were buried and left for one growing season each (2021 and 2023)

#### 4/13/25: data cleaned, and basic prelim plots made
#### by seeding treatment and drought treatment

library(tidyverse)

# data
nut21 <- read.csv("data/nutrients2021.csv", header=TRUE) #2021
nut21$plot <- as.factor(nut21$plot)
nut21$comm <- as.factor(nut21$comm)
nut21$drt <- as.factor(nut21$drt)
nut23 <- read.csv("data/nutrients2023.csv", header=TRUE) #2023
nut23$plot <- as.factor(nut23$plot)
nut23$comm <- as.factor(nut23$comm)
nut23$drt <- as.factor(nut23$drt)


anova(lm(TotalN ~ drt*comm, data=nut21))
m1 <- ggplot(nut21, aes(y=TotalN, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(NO3 ~ drt*comm, data=nut21))
m2 <- ggplot(nut21, aes(y=NO3, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(P ~ drt*comm, data=nut21))
m3 <- ggplot(nut21, aes(y=P, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(K ~ drt*comm, data=nut21))
m4 <- ggplot(nut21, aes(y=K, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(Ca ~ drt*comm, data=nut21))
m5 <- ggplot(nut21, aes(y=Ca, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(Mg ~ drt*comm, data=nut21))
m6 <- ggplot(nut21, aes(y=Mg, x=comm, fill=drt))+
  geom_boxplot()

#combine plot
library(ggpubr)
ggarrange(m1,m2,m3,m4,m5,m6, 
          nrows=2, common.legend = T)

#23
anova(lm(TotalN ~ drt*comm, data=nut23))
m7 <- ggplot(nut23, aes(y=TotalN, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(NO3 ~ drt*comm, data=nut23))
m8 <- ggplot(nut23, aes(y=NO3, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(P ~ drt*comm, data=nut23))
m9 <- ggplot(nut23, aes(y=P, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(K ~ drt*comm, data=nut23))
m10 <- ggplot(nut23, aes(y=K, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(Ca ~ drt*comm, data=nut23))
m11 <- ggplot(nut23, aes(y=Ca, x=comm, fill=drt))+
  geom_boxplot()

anova(lm(Mg ~ drt*comm, data=nut23))
m12 <- ggplot(nut23, aes(y=Mg, x=comm, fill=drt))+
  geom_boxplot()

#combine plot
ggarrange(m7,m8,m9,m10,m11,m12, 
          nrows=2, common.legend = T)
