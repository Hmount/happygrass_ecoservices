#### Calculating Euclidean distances to assess dissimilarity of community 
#### CWM traits and Roa/Q traits from trait targets in each plot.
#### Traits are still considered as achieving the target if they exceed the 
#### upper quantile or are below the minimum quantile (because minimum + 
#### maximum trait values should achieve their target even better than 
#### upper/lower quantile, respectively.). Targets are calculated within 
#### each year, and the functionally diverse goal was allowed to vary by 
#### year because different maximum FD was possible at each site each year 
#### by virtue of the species pool present. 
#### Wyoming ####

# packages used
library(tidyverse)
library(lme4)
library(lmerTest)
library(vegan)

# Function for normalizing FD
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#### load and clean data
comms22 <- read.csv("data/communities/validCWM22.csv")
comms22$year <- "2022"
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
comms <- bind_rows(comms22,comms23)
comms$year <- as.factor(comms$year)
comms$block <- as.factor(comms$block)
comms$trt <- as.factor(comms$trt)
comms$drought <- as.factor(comms$drought)
commsFD22 <- read.csv("data/communities/FD22.csv")
commsFD22$year <- "2022"
commsFD23 <- read.csv("data/communities/FD23.csv")
commsFD23$year <- "2023"
commsFD <- bind_rows(commsFD22,commsFD23)
commsFD$year <- as.factor(commsFD$year)
commsFD$block <- as.factor(commsFD$block)
commsFD$trt <- as.factor(commsFD$trt)
commsFD$drought <- as.factor(commsFD$drought)
# alldat <- read.csv("data/cwm_wy(plot).csv")
# alldat$year <- as.factor(alldat$year)
# alldat$block <- as.factor(alldat$block)
# alldat$trt <- as.factor(alldat$trt)
# FDdat <- read.csv("data/cwm_raoq_wy(plot).csv")
# FDdat$year <- as.factor(FDdat$year)
# FDdat$block <- as.factor(FDdat$block)
# FDdat$trt <- as.factor(FDdat$trt)
# CSV of species-trait combinations (for OG 25)
# trait data (see trait_wrangling.R)
traits <- read.csv("data/traits/mastertrait.csv")

# #scale traits
# traits$srl = scale(log(traits$srl))
# traits$ldmc = scale(log(traits$ldmc))
# traits$leafn = scale(log(traits$leafn))
# #traits$lop = scale(traits$lop)
# traits$rootdiam = scale(log(traits$rootdiam))
# traits$sla = scale(log(traits$sla))
# traits$rdmc = scale(log(traits$rdmc))

# traits.wy <- read.csv("data/trait_data/mixedgrass.csv", header=TRUE, row.names=1)
# traits.wy <- traits.wy[traits.wy$use==1,] # subset use=1
# traits.wy$PLSg.m2.mono <- traits.wy$PLSlbs.acre.mono * (453.59237 / 4046.8564224) #convert lb/acre to g/m2
#scale traits
traits$srl = scale(log(traits$srl))
traits$ldmc = scale(log(traits$ldmc))
traits$leafn = scale(log(traits$leafn))
traits$lop = scale(traits$lop)
traits$rootdiam = scale(log(traits$rootdiam))
#traits$sla = scale(log(traits$sla))
#traits$rdmc = scale(log(traits$rdmc))

## select relevent CWM traits per DT or IR, rows are communities
# remove predictor rows 
# pred <- alldat %>% filter(year=="0")#filter preds
# dat <- alldat %>% filter(year!="0")#filter preds
# FDpred <- FDdat %>% filter(year=="0")#filter preds
# FDdat <- FDdat %>% filter(year!="0")#filter preds

#### Find distance
#### subset for just seeding trt of interest and calculate dissimilarity
#### First distance to exact quantile-based target was used (commented out),
#### but instead we now find distance to the max/min of trait quantiles 
#### because communities still achieved if the target is above the upper quantile
#### or below the lower qualtile (ex. SRL above our target is even better
#### than hitting our target). We use the max/min distances for subsequent analyses.

## DT
distdt <- comms %>% select(c(block,trt,year,ldmc,lop))
distdt <- merge(distdt,commsFD[,c("plot","rootdiam","full","block","trt","year")], all.x = T)
# define trait targets
quantile(traits[c(1:25),]$ldmc,.75) # DT target was based off of OG 25 plants only
# 0.6766338 (old number, why different?)
# 0.8247699
quantile(traits[c(1:25),]$lop,.25) #DT
# -0.7420366 (old, why different?)
# -0.8492899 

## DT (min/max)
#dtdist <- distdt.wy %>% filter(trt=="dt")'
#distdt$rootdiam <- #normalize(distdt.wy$rootdiam)
##2021
# within each year subset the data
# dtdist21 <- distdt %>% filter(year=="2021")
# dtdist21 <- dtdist21 %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=T)
# # attach row of targets
# dtdist21 <- dtdist21 %>% add_row(trt.b.y = "target",
#                                  ldmc = quantile(dtdist21$ldmc,.95),
#                                  lop = quantile(dtdist21$lop,.05),
#                                  rootdiam = quantile(dtdist21$rootdiam,.95))
# dtdist21 <- dtdist21 %>% column_to_rownames("trt.b.y")
# #run dist or vegdist
# dtdistmat <- vegdist(as.matrix(dtdist21),method = "euclidean", upper=T)#,diag=T)
# dtdistmat <-as.matrix(dtdistmat)
# # save only padtwise between target
# dtdistances21 <- as.data.frame(dtdistmat["target",])
# colnames(dtdistances21) <- "distdt"
##2022
# within each year subset the data
dtdist22 <- distdt %>% filter(year=="2022")
dtdist22 <- dtdist22 %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=T)
# attach row of targets
dtdist22 <- dtdist22 %>% add_row(trt.b.y = "target",
                                 ldmc = quantile(dtdist22$ldmc,.95),
                                 lop = quantile(dtdist22$lop,.05),
                                 rootdiam = quantile(dtdist22$rootdiam,.95))
dtdist22 <- dtdist22 %>% column_to_rownames("trt.b.y")
dtdist22 <- dtdist22[,-3] #remove plot column
dtdist22 <- dtdist22[,-4] #remove full column?
#run dist or vegdist
dtdistmat <- vegdist(as.matrix(scale(dtdist22)),method = "euclidean", upper=T)#,diag=T)
dtdistmat <-as.matrix(dtdistmat)
# save only padtwise between target
dtdistances22 <- as.data.frame(dtdistmat["target",])
colnames(dtdistances22) <- "distdt"
##2023
# within each year subset the data
dtdist23 <- distdt %>% filter(year=="2023")
dtdist23 <- dtdist23 %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=T)
# attach row of targets
dtdist23 <- dtdist23 %>% add_row(trt.b.y = "target",
                                 ldmc = quantile(dtdist23$ldmc,.95),
                                 lop = quantile(dtdist23$lop,.05),
                                 rootdiam = quantile(dtdist23$rootdiam,.95))
dtdist23 <- dtdist23 %>% column_to_rownames("trt.b.y")
dtdist23 <- dtdist23[,-3] #remove plot column
dtdist23 <- dtdist23[,-4] #remove full column?
#run dist or vegdist
dtdistmat <- vegdist(as.matrix(scale(dtdist23)),method = "euclidean", upper=T)
dtdistmat <-as.matrix(dtdistmat)
# save only padtwise between target
dtdistances23 <- as.data.frame(dtdistmat["target",])
colnames(dtdistances23) <- "distdt"
## bind dataframes
dtdistances.max <- bind_rows(dtdistances22,dtdistances23)
dtdistances.max <- dtdistances.max %>% rownames_to_column("trt.b.y")


## To assess similarity to highest multivariate FD we use upper 95th percentile
## of each years FD as the target to compare to.
#FD
fddist <- commsFD[,c(7:11)]# %>% filter(trt=="fd")
#FD target (shifting annually)
#quantile(normalize(fddist$full),.99)
#using max(), gives 1 in all years due to normalization, 99th quantile shows variation
#subdatFD <- fddist %>% group_by(year) %>% summarize(target = quantile(normalize(full),.99))
#quantile is higher when considering less plots (i.e. only FD) and more representative of the
# comparison between FD and target FD so using subsetted data to calculate dist
# fddist <- distfd.wy %>%
#   unite(trt.b.y, c(trt, block, subplot, year), sep = ".", remove=T) # make unique plot variable
#fddist$full <- normalize(fddist$full)

##2022
# within each year subset the data
fddist22 <- fddist %>% filter(year=="2022")
fddist22 <- fddist22 %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=T)
# attach row of targets
fddist22 <- fddist22 %>% add_row(trt.b.y = "target",
                                 full = quantile(fddist22$full,.99)) #this should be by year tho
fddist22 <- fddist22[,-3]#remove drought column
fddist22 <- fddist22 %>% column_to_rownames("trt.b.y")
#run dist or vegdist
fddistmat <- vegdist(as.matrix(fddist22),method = "euclidean", upper=T)#,diag=T)
fddistmat <-as.matrix(fddistmat)
# save only pairwise between target
fddistances22 <- as.data.frame(fddistmat["target",])
colnames(fddistances22) <- "distfd"

##2023
# within each year subset the data
fddist23 <- fddist %>% filter(year=="2023")
fddist23 <- fddist23 %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=T)
# attach row of targets
fddist23 <- fddist23 %>% add_row(trt.b.y = "target",
                                 full = quantile(fddist23$full,.99)) #this should be by year tho
fddist23 <- fddist23[,-3]#remove drought column
fddist23 <- fddist23 %>% column_to_rownames("trt.b.y")
#run dist or vegdist
fddistmat <- vegdist(as.matrix(fddist23),method = "euclidean", upper=T)
fddistmat <-as.matrix(fddistmat)
# save only pairwise between target
fddistances23 <- as.data.frame(fddistmat["target",])
colnames(fddistances23) <- "distfd"

#bind dataframes
fddistances <- bind_rows(fddistances22,fddistances23)
fddistances <- fddistances %>% rownames_to_column("trt.b.y")


#### for RC:
#### We cannot compare the random community to a trait-based target because we
#### established these communities with no target (using a log normal distribution).
#### Instead, we use the CWM traits that the random control community should have 
#### produced based on the relative abundance of species in the seed mix and 
#### compare that to the realized CWM's for all six traits each year. 
#### This is useful to calculate for RC because it provides a control relationship between
#### functional change and taxonomic change (bray-curtis) like we examine with the other 
#### treatment groups. 

# within each year subset the data
# rdist <- alldat %>% filter(trt=="rand") %>% arrange(year,block)#%>% filter(year=="2021")
# rdist0 <- rdist %>% filter(year=="0") %>% arrange(as.numeric(block))

rdist <- alldat %>% arrange(year,block)
rdist0 <- rdist %>% filter(year=="0") %>% arrange(as.numeric(block))

### get CWM of intended CWM from seeding random communities 
precommscwm <- read.csv("data/communities/cwm_wy.csv")
precommscwm <- precommscwm %>% filter(year==0&trt=="rand")
precommscwm <- precommscwm %>% dplyr::select(block, trt, year, drought,
                                             leafn, lop, ldmc, srl, rootdiam)
precommscwm$block <- as.factor(precommscwm$block)
precommscwm$year <- as.factor(precommscwm$year)
precommscwm$trt <- as.factor(precommscwm$trt)

#2022
rdist22 <- comms %>% filter(year=="2022"&trt=="r")
rdist22 <- rdist22 %>% select(-c(X, graminoid,rdmc,sla))
rdist22 <- bind_rows(precommscwm,rdist22)
rdist22 <- rdist22 %>% unite(trt.b.y, c(trt, block,year), sep = ".", remove=T) # make unique plot variable
rdist22 <- rdist22 %>% column_to_rownames("trt.b.y")
rdist22 <- rdist22 %>% select(-c(drought))
randdistmat <- vegdist(as.matrix(scale(rdist22)),method = "euclidean", diag = T)
rdist22 <-as.matrix(randdistmat)[,-c(1:64)]
rdist22 <-rdist22[c(1:64),]
rdist22<- data.frame(
  dist=diag(as.matrix(rdist22)),
  id=colnames(rdist22))

#2023
rdist23 <- comms %>% filter(year=="2023" & trt == "r")
rdist23 <- rdist23 %>% select(-c(X, graminoid,rdmc,sla))
rdist23 <- bind_rows(precommscwm,rdist23)
rdist23 <- rdist23 %>% unite(trt.b.y, c(trt, block,year), sep = ".", remove=T) # make unique plot variable
rdist23 <- rdist23 %>% column_to_rownames("trt.b.y")
rdist23 <- rdist23 %>% select(-c(drought))
randdistmat <- vegdist(as.matrix(scale(rdist23)),method = "euclidean", diag = T)
rdist23 <-as.matrix(randdistmat)[,-c(1:64)]
rdist23 <-rdist23[c(1:64),]
rdist23<- data.frame(
  dist=diag(as.matrix(rdist23)),
  id=colnames(rdist23))

# together
rdistances <- bind_rows(rdist22,rdist23)
colnames(rdistances) <- c("distr","trt.b.y")

#### combine using min/max in DT and IR
alldist <- merge(dtdistances.max,fddistances, all = T)
alldist <- merge(alldist,rdistances, all=T)

## create a  column for the distance of each community to its specific target
alldist2 <- alldist %>% mutate(targetdist = ifelse(str_sub(trt.b.y, 1, 2)=="dt",distdt,
                                                         ifelse(str_sub(trt.b.y, 1, 2)=="fd",distfd,distr)))
#export csv
write.csv(alldist2, "data/distances.csv", row.names = F)
alldist2 <- alldist2 %>% separate(trt.b.y, c("trt", "block","year"), sep = "\\.") # make unique plot variable
alldist2 <- alldist2 %>% filter(trt != "ir" & trt != "target")
alldist2 <- alldist2 %>% 
  mutate(trt = relevel(factor(trt), ref = "r"))
summary(lm(distdt~trt, alldist2))
ggplot(alldist2, aes(y=targetdist, x=trt))+
  geom_boxplot()
ggplot(alldist2, aes(y=distdt, x=trt))+
  geom_boxplot()+
  facet_wrap(~year)
ggplot(alldist2, aes(y=distfd, x=trt))+
  geom_boxplot()+
  facet_wrap(~year)
ggplot(alldist2, aes(y=distfd, x=distdt, col=trt))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~year)

# test plot
smdatsubt <- smcomms.cwm %>% unite(trt.b.y, c(seedtrt, Block, Year), sep = ".", remove=F) # make unique plot variable
smtest <- merge(smdatsubt, alldist2, by="trt.b.y", all.x = T)
smtest <- smtest %>% filter(seedtrt != "ir")# & seedtrt != "r")
ggplot(smtest, aes(x=distfd, y=moistureyr, col=droughttrt))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="distance to DT",y="Soil moisture", col="Precipitation
treatment")+
  #facet_wrap(~Year, scales = "free")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

smdatsubt <- smcomms.cwm %>% unite(trt.b.y, c(seedtrt, Block, Year), sep = ".", remove=F) # make unique plot variable
smtest <- merge(smdatsubt, alldist2, by="trt.b.y")
smtest <- smtest %>% mutate(drought3 = as.factor(ifelse(drought=="0"& Year=="2022","152",
                                                             ifelse(drought=="1"& Year=="2022","55",
                                                                    ifelse(drought=="0"& Year=="2023","282",
                                                                           ifelse(drought=="1"& Year=="2023","102", NA))))))


smtestfddist <- ggplot(smtest, aes(x=distfd, y=moistureyr, col=droughttrt))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="distance to DT",y="Soil moisture", col="Precipitation
treatment")+
  facet_wrap(~Year, scales = "fixed")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

proddatsubt <- prod.comms.cwm %>% unite(trt.b.y, c(trt, block, year), sep = ".", remove=F) # make unique plot variable
prodtest <- merge(proddatsubt, alldist2, by="trt.b.y")
prodtest <- prodtest %>% mutate(drought3 = as.factor(ifelse(drought2=="ambient"& year=="2022","152",
                                                             ifelse(drought2=="drought"& year=="2022","55",
                                                                    ifelse(drought2=="ambient"& year=="2023","282",
                                                                           ifelse(drought2=="drought"& year=="2023","102", NA))))))
prodtest <- prodtest %>% filter(trt != "ir" & trt != "r")
summary(lmer(totcov.plot~ distfd*year*drought+(1|block), prodtest))
prodtestfddist <- ggplot(prodtest, aes(x=distfd, y=totcov.plot, col=drought))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="distance to FD",y="Total absolute cover", col="Precipitation
treatment")+
  facet_wrap(~year, scales = "fixed")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

prodtestdtdist <- ggplot(prodtest, aes(x=distdt, y=totcov.plot, col=drought))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="distance to DT",y="Total absolute cover", col="Precipitation
treatment")+
  facet_wrap(~year, scales = "fixed")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

annotate_figure(
  ggarrange(smtrtplot, smtestdtdist, smtestfddist, 
            common.legend = T, nrow=1), 
  left = "Volumentric water content")


annotate_figure(
  ggarrange(covtrtplot, covldmcplot, covfullplot, #prodtestdtdist, #prodtestfddist, 
            common.legend = T, nrow=1), 
  left = "Total absolute plant cover")
