#### Productivty (ANPP)

library(tidyverse)

## load data
# community composition data
coverdat <- read.csv("data/communities/comp_wy_plot.csv")# community composition per subplot
coverdat <- coverdat %>% filter(year!="2020") #remove pre-treatment (2020)
coverdat$drought <- as.factor(coverdat$drought)
coverdat <- coverdat %>% mutate(drought2 = ifelse(drought=="0","ambient","drought"))
#coverdat$trt <- toupper(coverdat$trt)
coverdat$trt <- as.factor(coverdat$trt) 
coverdat <- coverdat %>% filter(trt !="ir")
coverdat <- coverdat %>%
  mutate(trt = relevel(trt, ref = "r"))
coverdat <- coverdat %>% unite("plot",block,trt, sep="_", remove=F)

summary(lm(totcov.plot ~ trt*drought2*year, data=coverdat)) #no seed trt differences
#random plot effect did not improve the model (convergence)
anova(lm(totcov.plot ~ trt*drought2*year, data=coverdat))

ggplot(coverdat, aes(x = trt, y = totcov.plot, color = drought2)) +
  stat_summary(fun = mean,
               fun.data = mean_se,
               geom = "pointrange",
               position = position_dodge(width = 0.3)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Total absolute cover", x = "Seeding treatment",
       color = "Precipitation Treatment") +
  theme_bw()+
  facet_wrap(~year)



  
  
  #### CWM's and FD and soil moisture
  comms21 <- read.csv("data/communities/validCWM21.csv")
  comms21$year <- "2021"
  comms22 <- read.csv("data/communities/validCWM22.csv")
  comms22$year <- "2022"
  comms <- bind_rows(comms21,comms22)
  comms23 <- read.csv("data/communities/validCWM23.csv")
  comms23$year <- "2023"
  comms <- bind_rows(comms,comms23)
  comms <- comms %>% filter(trt!="ir")
  #subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
  smcomms.cwm <- merge(coverdat,comms, 
                       by.x=c("block","trt","drought","year"), 
                       by.y = c("block","trt","drought","year"),
                       all.y=T)
  #smcomms.cwm <- smcomms.cwm %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable
  
  
  summary(lmer(totcov.plot ~ drought2*ldmc*year+(1|block), data=smcomms.cwm)) 
  covldmcplot <- ggplot(smcomms.cwm, aes(x=ldmc, y=totcov.plot, col=drought))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="CWM LDMC",y="Total absolute plant cover", col="Precipitation
treatment")+
     facet_wrap(~year, scales="free")+
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

  smcomms.cwm <- merge(coverdat,comms.fd, 
                       by.x=c("block","trt","drought","year"), 
                       by.y = c("block","trt","drought","year"),
                       all.y=T)
  #smcomms.fd <- smcomms.fd %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable
  
  summary(lmer(totcov.plot ~ drought2*rootdiam*year+(1|block), data=smcomms.cwm)) 
  covfdrdplot <- ggplot(smcomms.cwm, aes(x=rootdiam, y=totcov.plot, col=drought))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="FD Root diameter",y="Total absolute plant cover", col="Precipitation
treatment")+
    facet_wrap(~year, scales="free")+
    theme_bw()  
  
  summary(lmer(totcov.plot ~ drought2*full*year+(1|block), data=smcomms.cwm)) 
 covfullplot <- ggplot(smcomms.cwm, aes(x=full, y=totcov.plot, col=drought2))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="FD",y="Total absolute plant cover", col="Precipitation
treatment")+
    facet_wrap(~year, scales="free")+
    theme_bw()   
  
  
  library(ggpubr)
  ggarrange(covldmcplot, covfdrdplot, covfullplot, common.legend = T, nrow=3)
  