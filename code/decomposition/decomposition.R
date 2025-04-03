#### Decomposition wrangling and analysis
#### Teabags were buried and left for a little more than 90 days during the 
#### 2023 growing season to measure decomposition in the different treatments (subplot)
#### 4/3/25: data cleaned, and started analysis. Weak differences in
#### decomp between seed and precip trt (although diff in Roobios, AKA:
#### C storage, between FD (DT) compared to R in AMBIENT ONLY)

#### TO DO:
## do I need to add covers or something to accont for total species in plots?
## compare with microbial biomass/ soil moisture/ or litter/cover??

#packages
library(tidyverse)

#### Wrangling ####
#### load in data and clean
decomp_raw <- read.csv("data/Teabag_weights.csv", na.strings = "") #teabag data
#remove additional columns (had weight of double-labelled bags,but we will remove these rows)
decomp_raw <- decomp_raw %>% select(-c("X","X.1"))
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
summary(lm(diff~trt*drought,data=subgreen)) # no differences in decomp
ggplot(subgreen, aes(x=diff, fill=trt))+
  geom_boxplot()+
  facet_wrap(~drought)
ggplot(subgreen, aes(y=diff, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))

subroo <- decomp %>% filter(tea=="roobois")
anova(lm(diff~trt*drought,data=subroo)) # differences in decomp
summary(lm(diff~trt*drought,data=subroo)) # differences in decomp
##more decomp in ambient in FD (and DT) than R
##BUT less decomp in drought in FD (and DT) than R
ggplot(subroo, aes(x=diff, fill=trt))+
  geom_boxplot()+
  facet_wrap(~drought)
## BEST PLOT
ggplot(subroo, aes(y=diff, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))
### all communities differ from random (because of precip trt effect)
### more storage occurs in R communties and droughted seed trts
### more decomp (less storage) occurs in seed trts in ambient 


# 
# 
# #### with covers?
# cover <- read.csv("data/cover_estimates/hpg_total.csv")
# cover$trt <- toupper(cover$trt)
# cover <- cover %>% filter(year == "2023")
# alldat <- merge(decomp, cover[,c(1:4,12:14)], by.x=c("plot","trt"), by.y=c("block","trt"))
# 
# 
# subgreen <- alldat %>% filter(tea=="green") 
# summary(lm(diff~sub.tveg*trt,data=subgreen)) # small, but sig effect of species cover on decomp by trt
# summary(lm(diff~sub.tveg*trt*drought,data=subgreen)) # better with drought
# anova(lm(diff~sub.tveg*trt*drought,data=subgreen)) # all variables important
# summary(lm(diff~Litter*trt,data=subgreen)) # no effect of species cover on decomp by trts
# #viz
# ggplot(subgreen, aes(y=diff, x=sub.tveg, color=trt))+
#   #geom_point()+
#   geom_smooth(method="lm")+
#   facet_wrap(~drought)
# 
# subroo <- alldat %>% filter(tea=="roobois")
# summary(lm(diff~sub.tveg*trt,data=subroo)) # stronger sig effect of species cover on decomp by trt
# summary(lm(diff~sub.tveg*trt*drought,data=subroo)) # even better with drought (20%)
# anova(lm(diff~sub.tveg*trt*drought,data=subroo)) # all variables very important
# summary(lm(diff~Litter*trt,data=subroo)) # lower R2 w/ litter by trt than OG model
# #viz
# ggplot(subroo, aes(y=diff, x=sub.tveg, color=trt))+
#   #geom_point()+
#   geom_smooth(method="lm")+
#   facet_wrap(~drought)