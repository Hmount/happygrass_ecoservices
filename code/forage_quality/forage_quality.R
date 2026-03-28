#### Forage Quality wrangling
#### Forage samples were collected by aggregating leaves of the same species 
#### gathered from plots across the whole site. Separate samples were collected 
#### in drought and control for each species in 2023.
#### Estimate community level forage quality by weighting different forage
#### metrics by species abundance in the community. 

#packages
library(tidyverse)
library(stringr)
library(ggpubr)
library(FD)

### load in forage data and clean
foragedat <- read.csv("data/Feed1_20230913.csv") #forage data
# keep only columns necessary for analysis
foragedat <- foragedat %>% dplyr::select(Sample.ID, Feed.Description,  
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
  dplyr::select(-c(species,treatment)) %>%
  column_to_rownames("spcode")
foragematrix.a <- foragedat.a[order(rownames(foragedat.a)),] # Arrange alphabetically
foragedat.d <- foragedat %>% filter(treatment == "D") %>%
  dplyr::select(-c(species,treatment)) %>%
  column_to_rownames("spcode")
foragematrix.d <- foragedat.d[order(rownames(foragedat.d)),] # Arrange alphabetically

## seperate drought treatment and wrangle to matrix form
comms.a <- comms %>% filter(drought == "0") %>% column_to_rownames("trt.b.s")
commsmatrix.a <- comms.a[,order(colnames(comms.a))]
commsmatrix.a <- replace(commsmatrix.a, is.na(commsmatrix.a), 0)
test.a <- commsmatrix.a %>% dplyr::select(all_of(rownames(foragematrix.a))) #ONLY NATIVE 25 and grouping columns

comms.d <- comms %>% filter(drought == "1") %>% column_to_rownames("trt.b.s")
commsmatrix.d <- comms.d[,order(colnames(comms.d))]
commsmatrix.d <- replace(commsmatrix.d, is.na(commsmatrix.d), 0)
test.d <- commsmatrix.d %>% dplyr::select(all_of(rownames(foragematrix.d))) #ONLY NATIVE 25 and grouping columns


## find mean for each important forage metric weighted by abundance 
## for drought and ambient treatments
foragemean.a <- FD::functcomp(as.matrix(foragematrix.a), as.matrix(test.a))
foragemean.a$trt.b.s <- rownames(foragemean.a) #rename rownames a column
foragemean.a <- separate(foragemean.a, trt.b.s, into = c("trt", "block"), sep = "\\.") 
foragemean.a$drought <- "ambient"

foragemean.d <- FD::functcomp(as.matrix(foragematrix.d), as.matrix(test.d))
foragemean.d$trt.b.s <- rownames(foragemean.d) #rename rownames a column
foragemean.d <- separate(foragemean.d, trt.b.s, into = c("trt", "block"), sep = "\\.")
foragemean.d$drought <- "drought"

forageall23 <- rbind(foragemean.a, foragemean.d)
forageall23 <- forageall23 %>%
  mutate(trt = factor(trt)) %>%
  filter(trt != "ir") %>%
  mutate(trt = relevel(trt, ref = "r"))

## repeat calculation for 2022 (this year experienced ambient drought so
## only estimating forage quality from 2023 drought-treatment samples)
# ### load in community composition data for weighted means
# comms <- read.csv("data/communities/comp_wy_plot.csv")
# comms <- comms %>% unite(trt.b.s, c(trt, block), sep = ".", remove=F) # make unique plot variable
# comms <- comms %>% filter(year == "2022") #use only collection year
# 
# ## make forage matrix
# foragedat.d <- foragedat %>% filter(treatment == "D") %>%
#   select(-c(species,treatment)) %>%
#   column_to_rownames("spcode")
# foragematrix.22 <- foragedat.d[order(rownames(foragedat.d)),] # Arrange alphabetically
# 
# ## seperate drought treatment and wrangle to matrix form
# commsmatrix.d <- comms %>%
#   column_to_rownames("trt.b.s")
# 
# commsmatrix.d <- commsmatrix.d[,order(colnames(commsmatrix.d))]
# commsmatrix.d <- replace(commsmatrix.d, is.na(commsmatrix.d), 0)
# test.22 <- commsmatrix.d %>% select(all_of(rownames(foragematrix.22))) #ONLY NATIVE 25 and grouping columns
# 
# 
# ## find mean for each important forage metric weighted by abundance 
# ## for drought and ambient treatments
# foragemean.22 <- FD::functcomp(as.matrix(foragematrix.22), as.matrix(test.22))
# foragemean.22$trt.b.s <- rownames(foragemean.22) #rename rownames a column
# foragemean.22 <- separate(foragemean.22, trt.b.s, into = c("trt", "block"), sep = "\\.")
# foragemean.22 <- merge(foragemean.22, forageall23[,c(8:10)], 
#                        by=c("block","trt"))
# 
# forageall22 <- foragemean.22 %>%
#   mutate(trt = factor(trt)) %>%
#   filter(trt != "ir") %>%
#   mutate(trt = relevel(trt, ref = "r"))

# ### load in community composition data for weighted means
# comms <- read.csv("data/communities/comp_wy_plot.csv")
# comms <- comms %>% unite(trt.b.s, c(trt, block), sep = ".", remove=F) # make unique plot variable
# comms <- comms %>% filter(year == "2022") #use only collection year
# 
# ## make forage matrix
# foragedat.a <- foragedat %>% filter(treatment == "C"|is.na(treatment)|treatment == "S") %>%
#   select(-c(species,treatment)) %>%
#   column_to_rownames("spcode")
# foragematrix.a <- foragedat.a[order(rownames(foragedat.a)),] # Arrange alphabetically
# foragedat.d <- foragedat %>% filter(treatment == "D") %>%
#   select(-c(species,treatment)) %>%
#   column_to_rownames("spcode")
# foragematrix.d <- foragedat.d[order(rownames(foragedat.d)),] # Arrange alphabetically
# 
# ## seperate drought treatment and wrangle to matrix form
# comms.a <- comms %>% filter(drought == "0") %>% column_to_rownames("trt.b.s")
# commsmatrix.a <- comms.a[,order(colnames(comms.a))]
# commsmatrix.a <- replace(commsmatrix.a, is.na(commsmatrix.a), 0)
# test.a <- commsmatrix.a %>% select(all_of(rownames(foragematrix.a))) #ONLY NATIVE 25 and grouping columns
# 
# comms.d <- comms %>% filter(drought == "1") %>% column_to_rownames("trt.b.s")
# commsmatrix.d <- comms.d[,order(colnames(comms.d))]
# commsmatrix.d <- replace(commsmatrix.d, is.na(commsmatrix.d), 0)
# test.d <- commsmatrix.d %>% select(all_of(rownames(foragematrix.d))) #ONLY NATIVE 25 and grouping columns
# 
# 
# ## find mean for each important forage metric weighted by abundance 
# ## for drought and ambient treatments
# foragemean.a <- FD::functcomp(as.matrix(foragematrix.a), as.matrix(test.a))
# foragemean.a$trt.b.s <- rownames(foragemean.a) #rename rownames a column
# foragemean.a <- separate(foragemean.a, trt.b.s, into = c("trt", "block"), sep = "\\.") 
# foragemean.a$drought <- "ambient"
# 
# foragemean.d <- FD::functcomp(as.matrix(foragematrix.d), as.matrix(test.d))
# foragemean.d$trt.b.s <- rownames(foragemean.d) #rename rownames a column
# foragemean.d <- separate(foragemean.d, trt.b.s, into = c("trt", "block"), sep = "\\.")
# foragemean.d$drought <- "drought"
# 
# forageall22 <- rbind(foragemean.a, foragemean.d)
# forageall22 <- forageall22 %>%
#   mutate(trt = factor(trt)) %>%
#   filter(trt != "ir") %>%
#   mutate(trt = relevel(trt, ref = "r"))
# 
# 
# forageall23$year <- "2023"
# forageall22$year <- "2022"
# forageall <- rbind(forageall23, forageall22)

#create letters for plotting:
library(emmeans)
# library(lme4)
# library(lmerTest)

##### USING CRUDE PROTIEN AND 
## plot
#for plot together with other funciton
summary(rfvmod <- glmmTMB::glmmTMB(RFV ~ trt*drought+(1|block), forageall23)) #should have block
# summary(rfvmod <- glmmTMB::glmmTMB(crudeprotien ~ trt*drought+(1|block), forageall23)) #should have block
# summary(rfvmod <- glmmTMB::glmmTMB(TDN ~ trt*drought+(1|block), forageall23)) #should have block

#summary(rfvmod <- lmer(RFV ~ trt*drought*year+(1|block), forageall))

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
dttemp2 <- forageall23 %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(RFV,.8, na.rm = T), .groups = 'drop')
# Step 5: Merge the letter results with the y-position data
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
# Merge with the original data to get the final dataset
dttemp3 <- merge(forageall23, dttemp2, by = c("drought", "trt"), all = TRUE)

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
  #facet_wrap(~year)+
  theme_bw()


summary(cpmod <- glmmTMB::glmmTMB(crudeprotien ~ trt*drought+(1|block), forageall23))
emm_trt <- emmeans(cpmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall23 %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(crudeprotien,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall23, dttemp2, by = c("drought", "trt"), all = TRUE)

cptrtplot <- ggplot(dttemp3, aes(x=trt, y=crudeprotien, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="Crude Protien", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  #facet_wrap(~year)+
  theme_bw()


summary(dmmod <- glmmTMB::glmmTMB(drymatter ~ trt*drought+(1|block), forageall23)) #not sig.
emm_trt <- emmeans(dmmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall23 %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(drymatter,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall23, dttemp2, by = c("drought", "trt"), all = TRUE)

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


summary(tdnmod <- glmmTMB::glmmTMB(TDN ~ trt*drought+(1|block), forageall23))
emm_trt <- emmeans(tdnmod, ~ trt * drought *year)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall %>%
  group_by(trt, drought, year) %>%
  summarise(yposition = quantile(TDN,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt", "year"))
dttemp3 <- merge(forageall, dttemp2, by = c("drought", "trt", "year"), all = TRUE)

m4 <- ggplot(forageall23, aes(x=trt, y=TDN, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="TDN", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  facet_wrap(~year)+
  theme_bw()


summary(adfmod <- glmmTMB::glmmTMB(ADF ~ trt*drought, forageall23))
emm_trt <- emmeans(adfmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall23 %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(ADF,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall23, dttemp2, by = c("drought", "trt"), all = TRUE)

adftrtpplot <- ggplot(dttemp3, aes(x=trt, y=ADF, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", y="ADF", fill="Precipitation 
treatment")+
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = .85), 
            vjust = -1.75,
            hjust = .6,
            size=3)+
  #facet_wrap(~year)+
  theme_bw()


summary(ndfmod <- glmmTMB::glmmTMB(NDF ~ trt*drought+(1|block), forageall23))
emm_trt <- emmeans(ndfmod, ~ trt * drought)
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters)
letters_df <- as.data.frame(cld_res)
dttemp2 <- forageall23 %>%
  group_by(trt, drought) %>%
  summarise(yposition = quantile(NDF,.8, na.rm = T), .groups = 'drop')
dttemp2 <- merge(letters_df, dttemp2, by = c("drought", "trt"))
dttemp3 <- merge(forageall23, dttemp2, by = c("drought", "trt"), all = TRUE)

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
  #facet_wrap(~year)+
  theme_bw()

#combined for report
annotate_figure(ggarrange(m1,m2,m3,m4,m5,m6, common.legend = T, 
                          nrow=2, ncol=3),
                bottom = "Seeding Treatment")


### What about the relationship between CWM and services?
# comms22 <- read.csv("data/communities/validCWM22.csv")
# comms22$year <- "2022"
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
#comms <- bind_rows(comms22,comms23)
comms <- comms23
#subset comms to just have CWM's for 10 best blocks with  data
subcomms <- comms %>% filter(block %in% unique(forageall$plot))
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
foragecomms <- merge(forageall23,comms, by.x=c("block","trt"), by.y = c("block","trt"))
#foragecomms <- foragecomms %>% unite(plot, block, trt, sep=".")
  
#only signifigant nutreint~trait models retained below
#Total N
summary(glmmTMB::glmmTMB(RFV ~ drought.x*ldmc*year.x + (1|block), data=foragecomms)) 
summary(adfmod_ldmc<-glmmTMB::glmmTMB(ADF ~ drought.x*ldmc+ (1|block), data=foragecomms))
summary(cpmod_ldmc <- glmmTMB::glmmTMB(crudeprotien ~ drought.x*ldmc+ (1|block), data=foragecomms)) 
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*ldmc+ (1|block), data=foragecomms))

cpldmcplot<-ggplot(foragecomms, aes(x=ldmc, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment", x="CWM LDMC", y="Crude protien")+
  theme_classic()

adfldmcplot <-ggplot(foragecomms, aes(x=ldmc, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment", x="CWM LDMC", y="Acid detergent fiber")+
  theme_classic()
##



# cprdfd<-ggplot(foragecomms, aes(x=ldmc, y=RFV, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# adfrdfd<-ggplot(foragecomms, aes(x=ldmc, y=rootdiam, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# cpffd<-ggplot(foragecomms, aes(x=ldmc, y=full, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# adffd<-ggplot(foragecomms, aes(x=ldmc, y=full, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# # adfldmc<-ggplot(foragecomms, aes(x=ldmc, y=ADF, col=drought.x))+
# #   geom_point()+
# #   geom_smooth(method = "lm")+
# #   scale_color_manual(values=c("skyblue","tomato2"))+
# #   labs(col="Precipitation 
# # Treatment")+
# #   theme_bw()+
# #   facet_wrap(~year.x)
# 
# # dmldmc<-ggplot(foragecomms, aes(x=ldmc, y=drymatter, col=drought.x))+
# #   geom_point()+
# #   geom_smooth(method = "lm")+
# #   scale_color_manual(values=c("skyblue","tomato2"))+
# #   labs(col="Precipitation 
# # Treatment")+
# #   theme_bw()+
# #   facet_wrap(~year.x)
# 
# 
# summary(glmmTMB::glmmTMB(RFV ~ drought.x*leafn + (1|block), data=foragecomms)) 
# summary(glmmTMB::glmmTMB(ADF ~ drought.x*leafn+ (1|block), data=foragecomms)) 
# summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*leafn+ (1|block), data=foragecomms)) 
# summary(glmmTMB::glmmTMB(drymatter ~ drought.x*leafn+ (1|block), data=foragecomms))
# 
# rvfln<-ggplot(foragecomms, aes(x=leafn, y=RFV, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# cpln<-ggplot(foragecomms, aes(x=leafn, y=crudeprotien, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# adfln<-ggplot(foragecomms, aes(x=leafn, y=ADF, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year.x)
# 
# dmln<-ggplot(foragecomms, aes(x=leafn, y=drymatter, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# 
# summary(glmmTMB::glmmTMB(RFV ~ drought.x*sla + (1|block), data=foragecomms)) 
# summary(glmmTMB::glmmTMB(ADF ~ drought.x*sla+ (1|block), data=foragecomms))
# summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*sla+ (1|block), data=foragecomms))
# summary(glmmTMB::glmmTMB(drymatter ~ drought.x*sla+ (1|block), data=foragecomms))
# 
# rvfsla<-ggplot(foragecomms, aes(x=sla, y=RFV, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm", se=F)+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()#+
# #facet_wrap(~year.x)
# cpsla<-ggplot(foragecomms, aes(x=sla, y=crudeprotien, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# adfsla<-ggplot(foragecomms, aes(x=sla, y=ADF, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# dmsla<-ggplot(foragecomms, aes(x=sla, y=drymatter, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm", se=F)+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(col="Precipitation 
# Treatment")+
#   theme_bw()
# 
# ggarrange(rvfldmc,cpldmc,adfldmc,dmldmc,
#           rvfln,cpln,adfln,dmln,
#           rvfsla,cpsla,adfsla,dmsla,
#           ncol=4, nrow=3, common.legend = T)
# 

### FD
# comms22.fd <- read.csv("data/communities/FD22.csv")
# comms22.fd$year <- "2022"
# comms22.fd$X <- as.factor(comms22.fd$X)
comms23.fd <- read.csv("data/communities/FD23.csv")
comms23.fd$year <- "2023"
comms23.fd$X <- as.factor(comms23.fd$X)
comms.fd <- comms23.fd
#comms.fd <- bind_rows(comms22.fd,comms23.fd)
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
foragecomms.fd <- merge(forageall23,comms.fd, by.x=c("block","trt"), by.y = c("block","trt"), all.x=T)
#foragecomms <- foragecomms %>% unite(plot, block, trt, sep=".")
# summary(glmmTMB::glmmTMB(RFV ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
# fdmod1 <- ggplot(foragecomms.fd, aes(x=ldmc, y=RFV, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD ldmc",col="Precipitation 
# Treatment")+
#   theme_bw()+
#   facet_wrap(~year)

#summary(cpmod_full<-glmmTMB::glmmTMB(crudeprotien ~ drought.x*rootdiam + (1|block), data=foragecomms.fd)) 
cpfdrdplot <- ggplot(foragecomms.fd, aes(x=rootdiam, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD root diameter",col="Precipitation 
Treatment")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

#summary(adfmod_full<-glmmTMB::glmmTMB(ADF ~ drought.x*rootdiam + (1|block), data=foragecomms.fd)) 
adffdrdplot <- ggplot(foragecomms.fd, aes(x=rootdiam, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD root diameter",col="Precipitation 
Treatment")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())


summary(cpmod_ldmc <- glmmTMB::glmmTMB(crudeprotien ~ drought.x*full + (1|block), data=foragecomms.fd)) 
cpfullplot <- ggplot(foragecomms.fd, aes(x=full, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD",col="Precipitation 
Treatment")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

summary(adfmod_full <- glmmTMB::glmmTMB(ADF ~ drought.x*full + (1|block), data=foragecomms.fd)) 
adffullplot <- ggplot(foragecomms.fd, aes(x=rootdiam, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x= "FD",col="Precipitation 
Treatment")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

# summary(glmmTMB::glmmTMB(drymatter ~ drought.x*ldmc + (1|block), data=foragecomms.fd)) 
# fdmod4 <- ggplot(foragecomms.fd, aes(x=ldmc, y=drymatter, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm", se=F)+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD ldmc",col="Precipitation 
# Treatment")+
#   theme_bw()#+

# summary(glmmTMB::glmmTMB(RFV ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
# fdmod5 <- ggplot(foragecomms.fd, aes(x=leafn, y=RFV, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD leaf nitrogen",col="Precipitation 
# Treatment")+
#   theme_bw()#+
# summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
# fdmod6 <- ggplot(foragecomms.fd, aes(x=leafn, y=crudeprotien, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD leaf nitrogen",col="Precipitation 
# Treatment")+
#   theme_bw()#+
# summary(glmmTMB::glmmTMB(ADF ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
# fdmod7 <- ggplot(foragecomms.fd, aes(x=leafn, y=ADF, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD leaf nitrogen",col="Precipitation 
# Treatment")+
#   theme_bw()#+
# summary(glmmTMB::glmmTMB(drymatter ~ drought.x*leafn + (1|block), data=foragecomms.fd)) 
# fdmod8 <- ggplot(foragecomms.fd, aes(x=leafn, y=drymatter, col=drought.x))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_color_manual(values=c("skyblue","tomato2"))+
#   labs(x= "FD leaf nitrogen", col="Precipitation 
# Treatment")+
#   theme_bw()#+

ggarrange(rvfldmc,cpldmc,adfldmc,dmldmc,
          rvfln,cpln,adfln,dmln,
          rvfsla,cpsla,adfsla,dmsla,
          fdmod1, fdmod2, fdmod3, fdmod4,
          fdmod5, fdmod6, fdmod7, fdmod8,
          ncol=4, nrow=5, common.legend = T)
