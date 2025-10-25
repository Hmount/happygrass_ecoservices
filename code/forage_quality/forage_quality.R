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
ggplot(foragedat, aes(x=RFV, y=spcode, shape=treatment))+
  geom_point()
ggplot(foragedat, aes(x=crudeprotien, y=spcode, shape=treatment))+
  geom_point()
ggplot(foragedat, aes(x=ADF, y=spcode, shape=treatment))+
  geom_point()

### load in community composition data for weighted means
comms <- read.csv("data/communities/comp_wy.csv")
comms <- comms %>% unite(trt.b.s, c(trt, block, subplot), sep = ".", remove=F) # make unique plot variable
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

## plot
m1 <- ggplot(forageall, aes(x=trt, y=RFV, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

m2 <- ggplot(forageall, aes(x=trt, y=crudeprotien, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

m3 <- ggplot(forageall, aes(x=trt, y=drymatter, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

m4 <- ggplot(forageall, aes(x=trt, y=TDN, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

m5 <- ggplot(forageall, aes(x=trt, y=ADF, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

m6 <- ggplot(forageall, aes(x=trt, y=NDF, fill=drought)) +
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(x=" ", fill="Precipitation 
Treatment")

#combined for report
annotate_figure(ggarrange(m1,m2,m3,m4,m5,m6, common.legend = T, 
                          nrow=2, ncol=3),
                bottom = "Seeding Treatment")


### What about the relationship between CWM and services?
comms21 <- read.csv("data/communities/validCWM21.csv")
comms21$year <- "2021"
comms23 <- read.csv("data/communities/validCWM23.csv")
comms23$year <- "2023"
comms <- bind_rows(comms21,comms23)
#subset comms to just have CWM's for 10 best blocks with nutrient data
subcomms <- comms23 %>% filter(block %in% unique(forageall$plot))
#test <- subcomms %>% group_by(block,trt,year) %>%
#merge with nutrient data
#subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
foragecomms <- merge(forageall,comms23, by.x=c("block","trt","subplot"), by.y = c("block","trt","subplot"))
foragecomms <- foragecomms %>% unite(plot, block, trt, sep=".")
  
#only signifigant nutreint~trait models retained below
#Total N
summary(glmmTMB::glmmTMB(RFV ~ drought.x*ldmc + (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(ADF ~ drought.x*ldmc+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*ldmc+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*ldmc+ (1|plot), data=foragecomms)) #only ldmc

rvfldmc<-ggplot(foragecomms, aes(x=ldmc, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")#+
  #facet_wrap(~year.x)
cpldmc<-ggplot(foragecomms, aes(x=ldmc, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
adfldmc<-ggplot(foragecomms, aes(x=ldmc, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
dmldmc<-ggplot(foragecomms, aes(x=ldmc, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")

summary(glmmTMB::glmmTMB(RFV ~ drought.x*leafn + (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(ADF ~ drought.x*leafn+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*leafn+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*leafn+ (1|plot), data=foragecomms)) #only ldmc

rvfln<-ggplot(foragecomms, aes(x=leafn, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")#+
#facet_wrap(~year.x)
cpln<-ggplot(foragecomms, aes(x=leafn, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
adfln<-ggplot(foragecomms, aes(x=leafn, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
dmln<-ggplot(foragecomms, aes(x=leafn, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")

summary(glmmTMB::glmmTMB(RFV ~ drought.x*sla + (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(ADF ~ drought.x*sla+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*sla+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*sla+ (1|plot), data=foragecomms)) #only ldmc

rvfsla<-ggplot(foragecomms, aes(x=sla, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")#+
#facet_wrap(~year.x)
cpsla<-ggplot(foragecomms, aes(x=sla, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
adfsla<-ggplot(foragecomms, aes(x=sla, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
dmsla<-ggplot(foragecomms, aes(x=sla, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")


summary(glmmTMB::glmmTMB(RFV ~ drought.x*lop + (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(ADF ~ drought.x*lop+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(crudeprotien ~ drought.x*lop+ (1|plot), data=foragecomms)) #only ldmc
summary(glmmTMB::glmmTMB(drymatter ~ drought.x*lop+ (1|plot), data=foragecomms)) #only ldmc

rvflop<-ggplot(foragecomms, aes(x=lop, y=RFV, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")#+
#facet_wrap(~year.x)
cplop<-ggplot(foragecomms, aes(x=lop, y=crudeprotien, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
adflop<-ggplot(foragecomms, aes(x=lop, y=ADF, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")
dmlop<-ggplot(foragecomms, aes(x=lop, y=drymatter, col=drought.x))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(col="Precipitation 
Treatment")


test<-ggarrange(rvfldmc,cpldmc,adfldmc,dmldmc,
          rvfln,cpln,adfln,dmln,
          rvfsla,cpsla,adfsla,dmsla,
          rvflop,cplop,adflop,dmlop,
          ncol=4, nrow=4, common.legend = T)
