#### Forage Quality wrangling
#### Forage samples were collected by aggregating leaves of the same species 
#### gathered from plots across the whole site. Separate samples were collected 
#### in drought and control for each species in 2023.
#### Estimate community level forage quality by weighting different forage
#### metrics by species abundance in the community. 

#packages
library(tidyverse)
library(stringr)

### load in forage data and clean
foragedat <- read.csv("data/Feed1_20230913.csv") #forage data
# keep only columns necessary for analysis
foragedat <- foragedat %>% select(Sample.ID, Feed.Description,  
                             Moisture...As.Received..Oven., #probably useless because I dried it before analysis
                             Dry.Matter...As.Received..Oven., #probably useless because I dried it before analysis
                             Crude.Protein...Dry.Basis,
                             ADF...Dry.Basis,
                             NDF...Dry.Basis,
                             TDN...Dry.Basis..ADF.,
                             RFV)
#rename columns
colnames(foragedat) <- c("ID","species","moisture","drymatter","crudeprotien",
                         "ADF","NDF","TDN","RFV")
foragedat <- foragedat %>%
  separate(ID, into = c("spcode", "treatment"), sep = "-")
# fix misnamed species 
foragedat$spcode <- str_replace(foragedat$spcode, "BRITE", "BRTE")
foragedat$spcode <- str_replace(foragedat$spcode, "ARDR", "ARFR")

## check how different feed values are between species in drought/ control
ggarrange(ggplot(foragedat, aes(x=RFV, y=spcode, col=treatment))+
  geom_point()+
    scale_color_manual(values=c("skyblue","tomato2","purple"), labels = c("ambient", "reduction","both"))+
    theme_bw(),
ggplot(foragedat, aes(x=crudeprotien, y=spcode, col=treatment))+
  geom_point()+
  scale_color_manual(values=c("skyblue","tomato2","purple"), labels = c("ambient", "reduction","both"))+
  theme_bw(),
ggplot(foragedat, aes(x=ADF, y=spcode, col=treatment))+
  geom_point()+
  scale_color_manual(values=c("skyblue","tomato2","purple"), labels = c("ambient", "reduction","both"))+
  theme_bw(),
ggplot(foragedat, aes(x=moisture, y=spcode, col=treatment))+
  geom_point()+
  scale_color_manual(values=c("skyblue","tomato2","purple"), labels = c("ambient", "reduction","both"))+
  theme_bw(),
common.legend = T)

### load in community composition data for weighted means
comms <- read.csv("data/communities/comp_wy_plot.csv")
comms <- comms %>% unite(trt.b.s, c(trt, block), sep = ".", remove=F) # make unique plot variable
comms <- comms %>% filter(year == "2023") #use only collection year

## make forage matrix
foragedat.a <- foragedat %>% filter(treatment == "C"|is.na(treatment)|treatment == "S") %>%
  select(-c(species,treatment)) %>%
  column_to_rownames("spcode")
foragematrix.a <- foragedat.a[order(rownames(foragedat.a)),] # Arrange alphabetically
foragedat.d <- foragedat %>% filter(treatment == "D") %>%
  select(-c(species,treatment)) %>%
  column_to_rownames("spcode")
foragematrix.d <- foragedat.d[order(rownames(foragedat.d)),] # Arrange alphabetically

## seperate drought treatment and wrangle to matrix form
comms.a <- comms %>% filter(drought == "0") %>% column_to_rownames("trt.b.s")
commsmatrix.a <- comms.a[,order(colnames(comms.a))]
commsmatrix.a <- replace(commsmatrix.a, is.na(commsmatrix.a), 0)
test.a <- commsmatrix.a %>% select(all_of(rownames(foragematrix.a))) #ONLY NATIVE 25 and grouping columns

comms.d <- comms %>% filter(drought == "1") %>% column_to_rownames("trt.b.s")
commsmatrix.d <- comms.d[,order(colnames(comms.d))]
commsmatrix.d <- replace(commsmatrix.d, is.na(commsmatrix.d), 0)
test.d <- commsmatrix.d %>% select(all_of(rownames(foragematrix.d))) #ONLY NATIVE 25 and grouping columns


## find mean for each important forage metric weighted by abundance 
## for drought and ambient treatments
foragemean.a <- FD::functcomp(as.matrix(foragematrix.a), as.matrix(test.a))
foragemean.a$trt.b.s <- rownames(foragemean.a) #rename rownames a column
foragemean.a <- separate(foragemean.a, trt.b.s, into = c("trt", "block", "subplot"), sep = "\\.") 
foragemean.a$drought <- "ambient"

foragemean.d <- FD::functcomp(as.matrix(foragematrix.d), as.matrix(test.d))
foragemean.d$trt.b.s <- rownames(foragemean.d) #rename rownames a column
foragemean.d <- separate(foragemean.d, trt.b.s, into = c("trt", "block", "subplot"), sep = "\\.")
foragemean.d$drought <- "drought"

forageall <- rbind(foragemean.a, foragemean.d)
forageall <- forageall %>%
  mutate(trt = factor(trt)) %>%
  filter(trt != "ir") %>%
  mutate(trt = relevel(trt, ref = "r"))
#create letters for plotting:
library(emmeans)
library(lme4)
library(lmerTest)

## plot
summary(rfvmod <- lmer(RFV ~ trt*drought+(1|block), forageall))
## ~ seeding treatment
# Step 1: Get the emmeans for the interaction of trt, drought
emm_trt <- emmeans(rfvmod, ~ trt * drought)
## # Step 2: Obtain pairwise contrasts for the interaction
## contrast_trt <- contrast(emm_trt, method = "pairwise")
# Step 2: Generate the compact letter display using multcomp::cld
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
# Step 3: Convert the results to a data frame
letters_df <- as.data.frame(cld_res)
# Step 4: Create a temporary data frame with the desired y-position for plotting
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(RFV,.8, na.rm = T), .groups = 'drop')
# Step 5: Merge the letter results with the y-position data
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
# Merge with the original data to get the final dataset
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

#plot:
m1 <- ggplot(dttemp3, aes(y=RFV, x=trt, fill=drought))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="RFV", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()


summary(cpmod <- lmer(crudeprotien ~ trt*drought+(1|block), forageall))
emm_trt <- emmeans(cpmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(crudeprotien,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

m2 <- ggplot(dttemp3, aes(x=trt, y=crudeprotien, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="Crude Protien", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()


summary(dmmod <- lmer(drymatter ~ trt*drought+(1|block), forageall)) #not sig.
emm_trt <- emmeans(dmmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(drymatter,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

m3 <- ggplot(dttemp3, aes(x=trt, y=drymatter, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="Dry Matter", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()


summary(tdnmod <- lmer(TDN ~ trt*drought+(1|block), forageall))
emm_trt <- emmeans(tdnmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(TDN,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

m4 <- ggplot(dttemp3, aes(x=trt, y=TDN, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="TDN", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()


summary(adfmod <- lmer(ADF ~ trt*drought+(1|block), forageall))
emm_trt <- emmeans(adfmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(ADF,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

m5 <- ggplot(dttemp3, aes(x=trt, y=ADF, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="ADF", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()


summary(ndfmod <- lmer(NDF ~ trt*drought+(1|block), forageall))
emm_trt <- emmeans(ndfmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(NDF,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt"), all = TRUE)

m6 <- ggplot(dttemp3, aes(x=trt, y=NDF, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="NDF", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  theme_bw()

#combined for report
annotate_figure(ggarrange(m1,m2,m3,m4,m5,m6, common.legend = T, 
                          nrow=2, ncol=3),
                bottom = "Seeding Treatment")


### What about the relationship between CWM and services?
# comms21 <- read.csv("data/communities/validCWM21.csv")
# comms21$year <- "2021"
comms23 <- read.csv("data/communities/validCWM23.csv")
# comms23$year <- "2023"
# comms <- bind_rows(comms21,comms23)
#subset comms to just have CWM's for 10 best blocks with  data
subcomms <- comms23 %>% filter(block %in% unique(forageall$plot))
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
foragecomms <- merge(forageall,comms23, by.x=c("block","trt"), by.y = c("block","trt"))
#foragecomms <- foragecomms %>% unite(plot, block, trt, sep=".")
  
#only signifigant nutreint~trait models retained below
#Total N
summary(glmmTMB::glmmTMB(RFV ~ drought.x*ldmc + (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(ADF ~ drought.x*ldmc+ (1|block), data=foragecomms))
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*ldmc+ (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*ldmc+ (1|block), data=foragecomms))

rvfldmc<-ggplot(foragecomms, aes(x=ldmc, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()#+
  #facet_wrap(~year.x)
cpldmc<-ggplot(foragecomms, aes(x=ldmc, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
adfldmc<-ggplot(foragecomms, aes(x=ldmc, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
dmldmc<-ggplot(foragecomms, aes(x=ldmc, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()

summary(glmmTMB::glmmTMB(RFV ~ drought.x*leafn + (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(ADF ~ drought.x*leafn+ (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*leafn+ (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*leafn+ (1|block), data=foragecomms))

rvfln<-ggplot(foragecomms, aes(x=leafn, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()#+
#facet_wrap(~year.x)
cpln<-ggplot(foragecomms, aes(x=leafn, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
adfln<-ggplot(foragecomms, aes(x=leafn, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
dmln<-ggplot(foragecomms, aes(x=leafn, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()

summary(glmmTMB::glmmTMB(RFV ~ drought.x*sla + (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(ADF ~ drought.x*sla+ (1|block), data=foragecomms))
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*sla+ (1|block), data=foragecomms))
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*sla+ (1|block), data=foragecomms))

rvfsla<-ggplot(foragecomms, aes(x=sla, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()#+
#facet_wrap(~year.x)
cpsla<-ggplot(foragecomms, aes(x=sla, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
adfsla<-ggplot(foragecomms, aes(x=sla, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()
dmsla<-ggplot(foragecomms, aes(x=sla, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")+
  theme_bw()

ggarrange(rvfldmc,cpldmc,adfldmc,dmldmc,
          rvfln,cpln,adfln,dmln,
          rvfsla,cpsla,adfsla,dmsla,
          ncol=4, nrow=3, common.legend = T)


### FD
# comms21 <- read.csv("data/communities/validCWM21.csv")
# comms21$year <- "2021"
comms23.fd <- read.csv("data/communities/FD23.csv")
# comms23$year <- "2023"
# comms <- bind_rows(comms21,comms23)
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
foragecomms.fd <- merge(forageall,comms23.fd, by.x=c("block","trt"), by.y = c("block","trt"))
#foragecomms <- foragecomms %>% unite(plot, block, trt, sep=".")


summary(glmmTMB::glmmTMB(RFV ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
fdmod1 <- ggplot(foragecomms.fd, aes(x=ldmc, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD ldmc",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
fdmod2 <- ggplot(foragecomms.fd, aes(x=ldmc, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD ldmc",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(ADF ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
fdmod3 <- ggplot(foragecomms.fd, aes(x=ldmc, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD ldmc",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
fdmod4 <- ggplot(foragecomms.fd, aes(x=ldmc, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD ldmc",col="Precipitation 
Treatment")+
  theme_bw()#+



summary(glmmTMB::glmmTMB(RFV ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
fdmod5 <- ggplot(foragecomms.fd, aes(x=leafn, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD leaf nitrogen",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
fdmod6 <- ggplot(foragecomms.fd, aes(x=leafn, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD leaf nitrogen",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(ADF ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
fdmod7 <- ggplot(foragecomms.fd, aes(x=leafn, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD leaf nitrogen",col="Precipitation 
Treatment")+
  theme_bw()#+
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
fdmod8 <- ggplot(foragecomms.fd, aes(x=leafn, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD leaf nitrogen", col="Precipitation 
Treatment")+
  theme_bw()#+

ggarrange(rvfldmc,cpldmc,adfldmc,dmldmc,
          rvfln,cpln,adfln,dmln,
          rvfsla,cpsla,adfsla,dmsla,
          fdmod1, fdmod2, fdmod3, fdmod4,
          fdmod5, fdmod6, fdmod7, fdmod8,
          ncol=4, nrow=5, common.legend = T)
