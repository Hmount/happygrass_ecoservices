#### Decomposition wrangling and analysis
#### Teabags were buried and left for a little more than 90 days during the 
#### 2023 growing season to measure decomposition in the different treatments (subplot)
#### 4/3/25: data cleaned, and started analysis. Weak differences in
#### decomp between seed and precip trt (although diff in Roobios, AKA:
#### C storage, between FD (DT) compared to R in AMBIENT ONLY)

#packages
library(tidyverse)

#### Wrangling ####
#### load in data and clean
decomp_raw <- read.csv("data/Teabag_weights.csv", na.strings = "") #teabag data
#remove additional columns (had weight of double-labelled bags,but we will remove these rows)
decomp_raw <- decomp_raw %>% dplyr::select(-c("X","X.1"))
#rename columns
colnames(decomp_raw) <- c("ID","initial","final","tea","notes")
decomp_raw$final <- as.numeric(decomp_raw$final)

##how many teabags total had some issue?
sum(!is.na(decomp_raw$notes)) #111, 57 of these just had replaced tags so I'll keep them (below)

#filter data
decomp <- decomp_raw %>% filter(is.na(decomp_raw$notes) | notes == "replaced tag")

##how many of each tea type?
subgreen <- decomp %>% filter(tea=="green")
sum(!is.na(subgreen$final)) #remove 23 (214 replicates)

subroo <- decomp %>% filter(tea=="roobois")
sum(!is.na(subroo$final)) #remove 27 (194 replicates)


#### Calculate change in weight
decomp <- decomp %>% mutate(diff = final-initial)
decomp <- decomp %>% mutate(pml = 1- (final/initial))

#four teabags gained weight, must be error, remove
decomp <- decomp %>% filter(diff <= 0) #also removes all NA

## separate ID into plot and trt data
decomp <- separate(decomp, ID, into = c("plot", "trt"), sep = "-")

## combine drought variable
drought <- read.csv("data/drought blocks.csv")
decomp <- merge(decomp, drought, by.x="plot", by.y = "block")
decomp$drought <- as.factor(decomp$drought)

### ensure data is in correct format and leveled
decomp$plot <- as.factor(decomp$plot)
decomp$tea <- as.factor(decomp$tea)
decomp$trt <- as.factor(decomp$trt)
decomp$trt <- relevel(decomp$trt, ref = "R")
decomp <- decomp %>% filter(trt != "IR")

# how much data per trt
teagab_summary <- decomp %>% group_by(trt,tea) %>% summarise(count = n())

### vizualize
ggplot(decomp, aes(x=diff, fill=trt))+
  geom_boxplot()+
  facet_wrap(~tea)

ggplot(decomp, aes(x=diff, fill=drought))+
  geom_boxplot()+
  facet_grid(trt~tea)

#anova
#anova(lm(diff~trt*tea,data=decomp))

#### Initial analyses ####
subgreen <- decomp %>% filter(tea=="green") 
anova(lm(diff~trt*drought,data=subgreen)) # no differences in decomp
summary(decompgreenmod <- glmmTMB::glmmTMB(diff~trt*drought+(1|plot),data=subgreen)) # no differences in decomp
ggplot(subgreen, aes(x=diff, fill=trt))+
  geom_boxplot()+
  facet_wrap(~drought)
ggplot(subgreen, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))

subroo <- decomp %>% filter(tea=="roobois")
summary(decomproomod <- glmmTMB::glmmTMB(pml~trt*drought+(1|plot),data=subroo)) # differences in decomp
anova(lm(pml~trt*drought,data=subroo)) # differences in decomp
##more decomp in ambient in FD (and DT) than R
##BUT less decomp in drought in FD (and DT) than R
ggplot(subroo, aes(x=diff, fill=trt))+
  geom_boxplot()+
  facet_wrap(~drought)
## BEST PLOT
ggplot(subroo, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))
### all communities differ from random (because of precip trt effect)
### more storage occurs in R communties and droughted seed trts
### more decomp (less storage) occurs in seed trts in ambient 


# 
# 
# #### with covers?
cover <- read.csv("data/communities/comp_wy_plot.csv")
cover$trt <- toupper(cover$trt)
cover <- cover %>% filter(year == "2023")
alldat <- merge(decomp, cover[,c(1:3,62:65)], by.x=c("plot","trt"), by.y=c("block","trt"))
# 
# ggplot(alldat, aes(y=Litter, fill=trt))+
#   geom_boxplot()+
#   facet_wrap(~drought)
ggplot(alldat, aes(y=lit.plotmean, x=trt, fill = drought))+
  geom_boxplot()

ggplot(alldat, aes(y=totalcov, x=trt, fill = drought))+
  geom_boxplot()

subgreen <- alldat %>% filter(tea=="green") 
dat <- subgreen[complete.cases(subgreen[, c("pml", "trt", "drought", "lit.plotmean")]), ]
summary(mod2<-lmer(pml~trt*drought*lit.plotmean+(1|plot),data=dat)) #not sig. ~2% variance explained
summary(mod1<-lmer(pml~trt*drought+(1|plot),data=dat)) #not sig. ~2% variance explained
ggplot(subgreen, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))
anova(mod1,mod2)

summary(mod <- lmer(pml ~ trt*drought+(1|plot), subgreen))
emm_trt <- emmeans(mod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- subgreen %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(pml,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(subgreen, dttemp2, by = c("drought", "trt"), all = TRUE)

greenplot <- ggplot(dttemp3, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(labels=c("Ambient","Drought"), values=c("skyblue","tomato2"))+
  labs(fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  facet_wrap(~tea)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()
  
  

subrooibos <- alldat %>% filter(tea=="roobois") 
dat <- subrooibos[complete.cases(subrooibos[, c("pml", "trt", "drought", "lit.plotmean")]), ]
summary(mod3<-lmer(pml~trt*drought*lit.plotmean+(1|plot),data=dat))#sig. ~16% variance explained
summary(mod4<-lmer(pml~trt*drought+(1|plot),data=dat))
ggplot(subrooibos, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))
anova(mod3,mod4) #spatial random effect does improve model

summary(mod <- lmer(pml ~ trt*drought+(1|plot), subrooibos))
emm_trt <- emmeans(mod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- subrooibos %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(pml,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(subrooibos, dttemp2, by = c("drought", "trt"), all = TRUE)

rooplot<- ggplot(dttemp3, aes(y=pml, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(labels=c("Ambient","Drought"), values=c("skyblue","tomato2"))+
  labs(fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  facet_wrap(~tea)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()


#figure for report
ggarrange(greenplot, rooplot, common.legend = T)



### What about the relationship between CWM and services?
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
comms$trt <- factor(toupper(as.character(comms$trt)))
roocomms <- merge(subrooibos,subcomms, by.x=c("plot","trt"), by.y = c("block","trt"))
greencomms <- merge(subgreen,subcomms, by.x=c("plot","trt"), by.y = c("block","trt"))

#roo only
#sla
# summary(glmmTMB::glmmTMB(pml ~ drought.x*sla+
#                            (1|plot), data=roocomms)) #not sig
# modsla<-ggplot(roocomms, aes(x=sla, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm", lty=2)+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()

#ldmc
summary(glmmTMB::glmmTMB(pml ~ drought.x*ldmc+
                           (1|plot), data=roocomms)) #yes, sig.
rooldmcplot<-ggplot(roocomms, aes(x=ldmc, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment", y="Precent mass loss (rooibos)")+
  theme_classic()

summary(glmmTMB::glmmTMB(pml ~ drought.x*ldmc+
                           (1|plot), data=greencomms)) #yes, sig.
greenldmcplot<-ggplot(greencomms, aes(x=ldmc, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment",y="Precent mass loss (rooibos)")+
  theme_classic()

# #srl
# summary(glmmTMB::glmmTMB(pml ~ drought.x*srl+
#                            (1|plot), data=roocomms)) #yes sig.
# modsrl<-ggplot(roocomms, aes(x=srl, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# 
# #rdmc
# summary(glmmTMB::glmmTMB(pml ~ drought.x*rdmc+
#                            (1|plot), data=roocomms)) #yes sig.
# modrdmc<-ggplot(roocomms, aes(x=rdmc, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()


ggarrange(modsla,modldmc,modsrl,modrdmc, common.legend = T)



### FD traits
## What about the relationship between CWM and services?
comms23.fd <- read.csv("data/communities/FD23.csv")
comms23.fd$year <- "2023"
#test <- subcomms %>% group_by(block,trt,year) %>%
comms.fd <- comms23.fd
#merge with nutrient data
comms.fd$trt <- factor(toupper(as.character(comms.fd$trt)))
roocomms.fd <- merge(subrooibos,subcomms.fd, by.x=c("plot","trt"), by.y = c("block","trt"))
greencomms.fd <- merge(subgreen,subcomms.fd, by.x=c("plot","trt"), by.y = c("block","trt"))

#roo only
#sla
# summary(glmmTMB::glmmTMB(pml ~ drought.x*sla+
#                            (1|plot), data=roocomms)) #not sig
# modsla<-ggplot(roocomms, aes(x=sla, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm", lty=2)+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()

#FD RD
summary(glmmTMB::glmmTMB(pml ~ drought.x*rootdiam+
                           (1|plot), data=roocomms.fd)) #yes, sig.
roofdrdplot<-ggplot(roocomms.fd, aes(x=rootdiam, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment",y="Precent mass loss (rooibos)")+
  theme_classic()

summary(glmmTMB::glmmTMB(pml ~ drought.x*rootdiam+
                           (1|plot), data=greencomms.fd)) #yes, sig.
greenfdrdplot<-ggplot(greencomms.fd, aes(x=ldmc, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment", y="Precent mass loss (green)")+
  theme_classic()

#ldmc
summary(glmmTMB::glmmTMB(pml ~ drought.x*rootdiam+
                           (1|plot), data=roocomms.fd)) #yes, sig.
roofullplot<-ggplot(roocomms.fd, aes(x=rootdiam, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment", y="Precent mass loss (rooibos)")+
  theme_classic()

#FULL
summary(glmmTMB::glmmTMB(pml ~ drought.x*full+
                           (1|plot), data=greencomms.fd)) #yes, sig.
greenfullplot<-ggplot(greencomms.fd, aes(x=full, y=pml, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
  labs(col="Precipitation 
Treatment", y="Precent mass loss (green)")+
  theme_classic()
# #srl
# summary(glmmTMB::glmmTMB(pml ~ drought.x*srl+
#                            (1|plot), data=roocomms)) #yes sig.
# modsrl<-ggplot(roocomms, aes(x=srl, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# 
# #rdmc
# summary(glmmTMB::glmmTMB(pml ~ drought.x*rdmc+
#                            (1|plot), data=roocomms)) #yes sig.
# modrdmc<-ggplot(roocomms, aes(x=rdmc, y=pml, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"),labels = c("ambient", "reduction"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
