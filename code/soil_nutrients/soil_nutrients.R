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
nut21 <- nut21 %>%
  mutate(comm = relevel(comm, ref = "RA"))
nut23 <- read.csv("data/nutrients2023.csv", header=TRUE) #2023
nut23$plot <- as.factor(nut23$plot)
nut23$comm <- as.factor(nut23$comm)
nut23$drt <- as.factor(nut23$drt)
nut23 <- nut23 %>%
  mutate(comm = relevel(comm, ref = "R"))

### 21
### all (except K) are signifigantly different, but only by drt trt
summary(lm(TotalN ~ drt*comm, data=nut21)) #sig. diff. drt trt + IR
anova(lm(TotalN ~ drt*comm, data=nut21))
m1 <- ggplot(nut21, aes(y=TotalN, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))
  #scale_fill_manual(values=c("#482576B3", "#2A788EB3", "#43BF71B3", "#FDE725B3"))

summary(lm(NO3 ~ drt*comm, data=nut21)) #sig. diff. drt trt + IR
anova(lm(NO3 ~ drt*comm, data=nut21))
m2 <- ggplot(nut21, aes(y=NO3, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(P ~ drt*comm, data=nut21)) #sig. diff. drt trt
anova(lm(P ~ drt*comm, data=nut21))
m3 <- ggplot(nut21, aes(y=P, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(K ~ drt*comm, data=nut21)) #not sig. (but DT ?)
anova(lm(K ~ drt*comm, data=nut21))
m4 <- ggplot(nut21, aes(y=K, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(Ca ~ drt*comm, data=nut21)) #sig. diff. drt trt + drt*FD
anova(lm(Ca ~ drt*comm, data=nut21))
m5 <- ggplot(nut21, aes(y=Ca, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(Mg ~ drt*comm, data=nut21)) #sig. diff. drt trt + drt*FD
anova(lm(Mg ~ drt*comm, data=nut21))
m6 <- ggplot(nut21, aes(y=Mg, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

#combine plot
library(ggpubr)
annotate_figure(ggarrange(m1,m2,m3,m4,m5,m6, common.legend = T), top="2021")

### 23
### only P is signifigantly different
summary(lm(TotalN ~ drt*comm, data=nut23)) #not sig.
anova(lm(TotalN ~ drt*comm, data=nut23))
m7 <- ggplot(nut23, aes(y=TotalN, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(NO3 ~ drt*comm, data=nut23)) #not sig.
anova(lm(NO3 ~ drt*comm, data=nut23))
m8 <- ggplot(nut23, aes(y=NO3, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(P ~ drt*comm, data=nut23)) #sig. diff. drt trt
anova(lm(P ~ drt*comm, data=nut23))
m9 <- ggplot(nut23, aes(y=P, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(K ~ drt*comm, data=nut23)) #not sig.
anova(lm(K ~ drt*comm, data=nut23))
m10 <- ggplot(nut23, aes(y=K, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(Ca ~ drt*comm, data=nut23)) #not sig.
anova(lm(Ca ~ drt*comm, data=nut23))
m11 <- ggplot(nut23, aes(y=Ca, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

summary(lm(Mg ~ drt*comm, data=nut23)) #not sig.
anova(lm(Mg ~ drt*comm, data=nut23))
m12 <- ggplot(nut23, aes(y=Mg, x=comm, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato"))

#combine plot
annotate_figure(ggarrange(m7,m8,m9,m10,m11,m12, common.legend = T), top="2023")


### together?
nut23 <-nut23 %>% select(-X,-X.1,-X.2, -X..Anion, -X..Cation) %>% #remove empty columns
  mutate(year = "2023")
nut23$drt <- gsub("control", "ambient", nut23$drt)
nut21 <-nut21 %>% select(-Anion, -Cation) %>% #remove misnamed columns (just probe numbers)
  mutate(year = "2021")
nut21$comm <- gsub("RA", "R", nut21$comm)
nut21$drt <- gsub("amb", "ambient", nut21$drt)
nut21$drt <- gsub("drt", "drought", nut21$drt)

nut <- bind_rows(nut21,nut23)
nut$year <- as.factor(nut$year)
nut$drt <- as.factor(nut$drt)

#Total N
summary(lm(TotalN ~ drt*comm*year, data=nut)) #only drought and year matter
anova(lm(TotalN ~ drt*comm*year, data=nut)) #only drt*year interaction matters
m13<-ggplot(nut, aes(x=comm, y=TotalN, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)
  
#NO3
summary(lm(NO3 ~ drt*comm*year, data=nut)) #only drought and drought*year matter
anova(lm(NO3 ~ drt*comm*year, data=nut)) #only drt*year interaction matters
m14<-ggplot(nut, aes(x=comm, y=NO3, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)

#P
summary(lm(P ~ drt*comm*year, data=nut)) #poor midel, but some comm interactions
anova(lm(P ~ drt*comm*year, data=nut)) #drt,year,comm, drt*year, and year*comm  matters
m15<-ggplot(nut, aes(x=comm, y=P, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)
  
#K
summary(lm(K ~ drt*comm*year, data=nut)) #only drought and year matter
anova(lm(K ~ drt*comm*year, data=nut)) #only drt*year interaction matters
m16<-ggplot(nut, aes(x=comm, y=K, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)
  
#CA
summary(lm(Ca ~ drt*comm*year, data=nut)) #only drought and drought*year matter
anova(lm(Ca ~ drt*comm*year, data=nut)) #drt, year, and drt*year interaction matters
m17<-ggplot(nut, aes(x=comm, y=Ca, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)
  
#MG
summary(lm(Mg ~ drt*comm*year, data=nut)) # drought, drought* year, IR*2023
anova(lm(Mg ~ drt*comm*year, data=nut)) #drt, year, and drt*year interaction
m18<-ggplot(nut, aes(x=comm, y=Mg, fill=drt))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")+
  facet_wrap(~year)

#combined for report
annotate_figure(ggarrange(m13,m14,m15,m16,m17,m18, common.legend = T, 
                          nrow=3, ncol=2),
                bottom = "Seeding Treatment")


### What about the relationship between CWM and services?
comms21 <- read.csv("data/communities/validCWM21.csv")
comms21$year <- "2021"
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
comms <- bind_rows(comms21,comms23)
#subset comms to just have CWM's for 10 best blocks with nutrient data
subcomms <- comms %>% filter(block %in% unique(nut$plot))
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
nutcomms <- merge(nut,subcomms, by.x=c("plot","comm"), by.y = c("block","trt"))

## running models we expect traits to influence nutrients 
#Total N ~ leaf N (leaves uptake and return N to soil)
summary(glmmTMB::glmmTMB(TotalN ~ drt*leafn*year.x+
                           (1|plot), data=nutcomms)) 
m19<-ggplot(nutcomms, aes(x=leafn, y=TotalN, col=drt))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)

#Total N ~ srl (higher srl could relate to more root turnover or foraging in low N)
summary(glmmTMB::glmmTMB(TotalN ~ drt*srl*year.x+
             (1|plot), data=nutcomms)) #only ldmc
m20<-ggplot(nutcomms, aes(x=srl, y=TotalN, col=drt))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)


#P ~ leaf N (n and p are related in leaf chemistry and soil colimitation?)
summary(glmmTMB::glmmTMB(P ~ drt*leafn*year.x+
                           (1|plot), data=nutcomms)) #not sig.
ggplot(nutcomms, aes(x=TotalN, y=P, col=drt))+
  geom_point()+
  geom_smooth(method="lm", lty=2)+
  facet_wrap(~year.x)
m21<-ggplot(nutcomms, aes(x=leafn, y=P, col=drt))+
  geom_point()+
  geom_smooth(method = "lm", lty=2)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)

#P ~ srl (increased when foraging for P?)
summary(glmmTMB::glmmTMB(P ~ drt*srl*year.x+
                           (1|plot), data=nutcomms)) #not sig.
m22<-ggplot(nutcomms, aes(x=srl, y=P, col=drt))+
  geom_point()+
  geom_smooth(method = "lm", lty=2)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)


#Ca ~ ldmc (leaf structure, pos. cor.)
summary(glmmTMB::glmmTMB(Ca ~ drt*ldmc*year.x+
                           (1|plot), data=nutcomms)) #bearly sig interaction
m23<-ggplot(nutcomms, aes(x=ldmc, y=Ca, col=drt))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)

#Ca ~ rdmc (root structure?)
summary(glmmTMB::glmmTMB(Ca ~ drt*rdmc*year.x+
                           (1|plot), data=nutcomms)) #not sig.
m24<-ggplot(nutcomms, aes(x=rdmc, y=Ca, col=drt))+
  geom_point()+
  geom_smooth(method = "lm", lty=2)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  facet_wrap(~year.x)


#combined for report
annotate_figure(ggarrange(m19,m20,m21,m22,m23,m24, common.legend = T, 
                          nrow=3, ncol=2),
                bottom = "CWM trait")
