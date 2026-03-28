#### Productivity (ANPP)
#### just using 2023 for now (3/23/26)
#options(download.file.method = "wininet")

library(tidyverse)
library(emmeans)
library(glmmTMB)
library(multcomp)
#library(ggpubr)
#library(ggeffects)

## load data
# community composition data
coverdat <- read.csv("data/communities/comp_wy_plot.csv")# community composition per subplot
coverdat <- coverdat %>% filter(year!="2020") #remove pre-treatment (2020)
coverdat <- coverdat %>% filter(year!="2021") #keep only 2022 (dry) and 2023 (wet)
coverdat$year <- as.factor(coverdat$year)
coverdat$drought <- as.factor(coverdat$drought)
coverdat <- coverdat %>% mutate(drought2 = as.factor(ifelse(drought=="0","ambient","drought")))
coverdat <- coverdat %>% mutate(drought3 = as.numeric(ifelse(drought2=="ambient"& year=="2022","152",
                                                  ifelse(drought2=="drought"& year=="2022","55",
                                                         ifelse(drought2=="ambient"& year=="2023","282",
                                                                ifelse(drought2=="drought"& year=="2023","102", NA))))))
#coverdat$trt <- toupper(coverdat$trt)
coverdat$trt <- as.factor(coverdat$trt) 
coverdat <- coverdat %>% filter(trt !="ir") #with IR invasive cover differences don't change
coverdat <- coverdat %>%
  mutate(trt = relevel(trt, ref = "r"))
coverdat <- coverdat %>% unite("plot",block,trt, sep="_", remove=F)
coverdat <- coverdat %>% mutate(invasivecover = totcov.plot-nativecov.plot)

#### Modelling as a function of seeding treatment:
#### test productivity model (total summed absolute plot cover ~ trt)
covsub <- coverdat %>% filter(year=="2023")
covsub$block <- as.factor(covsub$block)
# model with block random effect
summary(covmod <- glmmTMB::glmmTMB(totcov.plot ~ trt*drought2+(1|block), data=covsub))

## figures
ggplot(covsub, aes(x = trt, y = totcov.plot, fill = drought2)) +
  geom_boxplot()+
  # stat_summary(fun = mean,
  #              fun.data = ggpubr::mean_sd,
  #              geom = "pointrange",
  #              position = position_dodge(width = 0.3)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(y = "Total absolute cover", x = "Seeding treatment",
       color = "Precipitation Treatment") +
  theme_bw()

## ~ seeding treatment
library(multcompView)
#create letters for plotting:
# Step 1: Get the emmeans for the interaction of trt, drought, and year
emm_trt <- emmeans(covmod, ~ trt * drought2) #| Year)
## # Step 2: Obtain pairwise contrasts for the interaction
## contrast_trt <- contrast(emm_trt, method = "pairwise")
# Step 2: Generate the compact letter display using multcomp::cld
cld_res <- multcomp::cld(emm_trt, adjust = "tukey", Letters=letters) #by=year
# Step 3: Convert the results to a data frame
letters_df <- as.data.frame(cld_res)
# Step 4: Create a temporary data frame with the desired y-position for plotting
dttemp2 <- coverdat %>%
  group_by(trt, drought2) %>%
  summarise(yposition = quantile(totcov.plot,.9, na.rm = T), .groups = 'drop')
# Step 5: Merge the letter results with the y-position data
dttemp2 <- merge(letters_df, dttemp2, by = c("drought2", "trt"))#, by.y = c("drought2", "trt"))
# Merge with the original data to get the final dataset
dttemp3 <- merge(coverdat, dttemp2, by = c("drought2", "trt"))#, by.y = c("drought", "trt"), all = TRUE)

# bocplot (appendix only)
covtrtplot <- ggplot(dttemp3, aes(x = trt, y = totcov.plot, fill = drought2)) +
  geom_boxplot()+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(y = "Total absolute cover", x = "Seeding treatment",
       fill = "Precipitation Treatment") +
  geom_text(aes(y=yposition,label = .group), 
            position = position_dodge(width = 1), 
            vjust = -1.75,
            hjust = .5,
            size=3)+
  theme_classic()+
  theme(axis.title.y.left = element_blank())#+
# facet_wrap(~year)


#### test invasive versus productivity model (total proportion invasive cover ~ trt)
### Also invader relative abundance/ proportion (better than total invasive
### cover because there were more invasive in the drought treatment in 2023
### due to open area in the drought plots from extreme ambient drought in 2022)
### proportion better captures the dynamics of invasive colonization
covsub <- covsub %>% mutate(propinv = invasivecover / totcov.plot)
#summary(covmod <- glmmTMB::glmmTMB(invasivecover ~ trt*drought2+(1|block), data=covsub))
summary(invmod <- glmmTMB::glmmTMB(propinv ~ trt*drought2+(1|block), data=covsub))
invtrtplot <- ggplot(covsub, aes(x = trt, y = propinv, fill = drought2)) +
  geom_boxplot()+
  # stat_summary(fun = mean,
  #              fun.data = mean_se,
  #              geom = "pointrange",
  #              position = position_dodge(width = 0.3)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("skyblue","tomato2"))+
  labs(y = "Proportion invasive cover", x = "Seeding treatment",
       color = "Precipitation Treatment") +
  theme_bw()

##examine invader richness (little to no difference, and a lot of variance 
## is being absorbed by plot, so not using)
#   covsub <- covsub %>%
#     rowwise() %>%
#     mutate(
#       richness = sum(c_across(38:62) != 0 & !is.na(c_across(38:62)))
#     ) %>%
#     ungroup()
# summary(covmod <- lmer(richness ~ trt*drought2+(1|block), data=covsub))



#### Modelling as a function of CWM (specifically LDMC)
  #### CWM's and FD and soil moisture
  # comms21 <- read.csv("data/communities/validCWM21.csv")
  # comms21$year <- "2021"
  # comms22 <- read.csv("data/communities/validCWM22.csv")
  # comms22$year <- "2022"
  #comms <- bind_rows(comms21,comms22)
  comms23 <- read.csv("data/communities/validCWM23.csv")
  comms23$year <- "2023"
  comms <- comms23
  #comms <- bind_rows(comms22,comms23)
  comms <- comms %>% filter(trt!="ir")
  #subcomms$trt <- factor(toupper(as.character(subcomms$trt)))
  prod.comms.cwm <- merge(covsub,comms, 
                       by.x=c("block","trt","drought"), 
                       by.y = c("block","trt","drought"),
                       all.y=T)
prod.comms.cwm$block <- as.factor(prod.comms.cwm$block)
  #smcomms.cwm <- smcomms.cwm %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable
  
### First, total productivity
summary(covmod_ldmc <- glmmTMB(totcov.plot ~ drought2*ldmc+(1|block), data=prod.comms.cwm)) 
covldmcplot <- ggplot(prod.comms.cwm, aes(x=ldmc, y=totcov.plot, col=drought2))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="CWM LDMC",y="Total absolute\n plant cover", col="Precipitation
treatment")+
    #facet_wrap(~year)+#, scales = "free_x")+
    theme_classic()

### Second, invasive relative abundance
summary(invmod_ldmc <- glmmTMB(propinv ~ drought2*ldmc+(1|block), data=prod.comms.cwm)) 
invldmcplot <- ggplot(prod.comms.cwm, aes(x=ldmc, y=propinv, col=drought2))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="CWM LDMC",y="Proportion \n invasive cover", col="Precipitation
treatment")+
  #facet_wrap(~year)+#, scales = "free_x")+
   theme_classic()#+
  # theme(axis.title.y.left = element_blank())
  

#### Modelling as a function of FD (specifically FD of root diameter (DT trt),
#### and overall FD (5-traits))
### What about the relationship between CWM and services?
  # comms21.fd <- read.csv("data/communities/FD21.csv")
  # comms21.fd$year <- "2021"
  # comms22.fd <- read.csv("data/communities/FD22.csv")
  # comms22.fd$year <- "2022"
  # comms.fd <- bind_rows(comms21.fd,comms22.fd)
comms23.fd <- read.csv("data/communities/FD23.csv")
comms23.fd$year <- "2023"
 # comms.fd <- bind_rows(comms22.fd,comms23.fd)
comms23.fd <- comms23.fd %>% filter(trt!="ir")
  
prod.comms.fd <- merge(covsub,comms23.fd, 
                       by.x=c("block","trt","drought"), 
                       by.y = c("block","trt","drought"),
                       all.y=T)
  #smcomms.fd <- smcomms.fd %>% unite(plot, c(Block, seedtrt), sep = "_", remove=F) # make unique plot variable
# anova(lmer(totcov.plot ~ drought2*rootdiam*year+(1|block), data=prod.comms.fd), 
#       lmer(totcov.plot ~ drought2*rootdiam*year+(1|plot.x), data=prod.comms.fd)) 
#   

### For root diam, first: total cover ~ FD root diameter
summary(covmod_rdfd <- glmmTMB(totcov.plot ~ drought2*rootdiam+(1|block), data=prod.comms.fd)) 
covrdfdplot <- ggplot(prod.comms.fd, aes(x=rootdiam, y=totcov.plot, col=drought2))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="FD RD",y="Total absolute plant cover", col="Precipitation
treatment")+
    #facet_wrap(~year)+#, scales = "free_x")+
    theme_classic()+
    theme(axis.title.y.left = element_blank())

### For root diam, second: invasive proportion ~ FD root diameter
summary(invmod_rdfd <- glmmTMB(propinv ~ drought2*rootdiam+(1|block), data=prod.comms.fd)) 
invrdfdplot <- ggplot(prod.comms.fd, aes(x=rootdiam, y=propinv, col=drought2))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="FD RD",y="Total absolute plant cover", col="Precipitation
treatment")+
  #facet_wrap(~year)+#, scales = "free_x")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())

### For full FD, first: total cover ~ FD
summary(covmod_full <- glmmTMB(totcov.plot ~ drought2*full+(1|block), data=prod.comms.fd)) 
covfullplot <- ggplot(prod.comms.fd, aes(x=full, y=totcov.plot, col=drought2))+
    geom_point()+
    geom_smooth(method="lm")+
  scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values=c("skyblue","tomato2"))+
    labs(x="FD",y="Total absolute plant cover", col="Precipitation
treatment")+
    #facet_wrap(~trt)+
    theme_classic() +
   theme(axis.title.y.left = element_blank())

### For full FD, second: invasive proportion ~ FD
cor.test(prod.comms.fd$full, prod.comms.fd$propinv) #probably needed to justify model
                                                    #since FD could be driven by invasive
                                                    #However, they are weakly correlated
#with water inv prop increases with greater FD likley a cause
# withough water, low FD = high inv cover would mean invasive dominate - they never dominate
summary(invmod_full <- glmmTMB(propinv ~ drought2*full+(1|block), data=prod.comms.fd)) 
invfullplot <- ggplot(prod.comms.fd, aes(x=full, y=propinv, col=drought2))+
  geom_point()+#aes(shape=trt))+
  geom_smooth(method="lm")+
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("skyblue","tomato2"))+
  labs(x="FD",y="Total absolute plant cover", col="Precipitation
treatment")+
  #facet_wrap(~trt)+
  theme_classic() +
  theme(axis.title.y.left = element_blank())
  mutate(droughttrt = recode(droughttrt,
                            "cntl" = "Ambient",
                            "drt"  = "Drought"))
#ldmc and root diam are independent, but full is .6 corr with both
cor(prod.comms.fd[, c("ldmc", "rootdiam", "full")])
plotlist <- c(covldmcplot, 
              covrdfdplot, 
              covfullplot)
annotate_figure(
  ggarrange(plotlist = plotlist,
            common.legend = T, nrow=1), 
  left = "Total absolute plant cover")
# library(patchwork)
# (covldmcplot / covfdrdplot / covfullplot) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")

# ##combined not better really
# prodcomms1 <- prod.comms.fd %>% dplyr::select(block, trt, drought2, rootdiam, full)
# prodcomms2 <- prod.comms.cwm %>% dplyr::select(block, trt, totcov.plot, ldmc)
# prodcomms<- merge(prodcomms1, prodcomms2, by=c("block","trt")) 
# summary(modt <- glmmTMB(totcov.plot ~ drought2*ldmc*rootdiam+(1|block), data=prodcomms)) 
# #fdpreds <- ggpredict(modt, terms = c("ldmc","rootdiam", "drought2"))  # or terms = c("xvar", "group")
# 

summary(m1<-lmer(totcov.plot ~ drought2*full*year+(1|plot.x), data=prod.comms.fd)) 
summary(m2<-lmer(totcov.plot ~ drought2*ldmc*year+(1|plot.x), data=prod.comms.fd))
summary(m3<-lmer(totcov.plot ~ drought2*trt*year+(1|plot.x), data=prod.comms.fd)) 
AIC(m1,m2,m3)
summary(lmer(cover ~ drought2*CWMLDMC*year+(1|block), data=prod.comms.fd)) 
summary(lmer(decomp ~ drought2*trt*year+(1|block), data=prod.comms.fd)) 



