#### figure to compare standard effect sizes for all functions
#### by seed trts (no traits yet)

library(ggpubr)
library(parameters)
library(patchwork)
## New Figures
##
##
par_prod = standardize_parameters(covmod, method = "refit") %>% mutate(env = "Productivity")
par_inv = standardize_parameters(invmod, method = "refit") %>% mutate(env = "Invasive Proportion")
par_forage = standardize_parameters(rfvmod, method = "refit") %>% mutate(env = "Forage Quality")
par_prod$Parameter <- par_forage$Parameter
par_inv$Parameter <- par_forage$Parameter
par_fdis1 = rbind(par_prod, par_inv, par_forage) %>% 
  mutate(shape = ifelse(CI_low <= 0 & CI_high >= 0, NA, "circle"),
          color_line = c(rep("black", 6), rep("darkgrey", 6), rep("grey", 6)),
          color_point = c(rep("black", 6), rep("darkgrey", 6), rep("grey", 6)),
          point_alpha = 1
  )
par_fdis1[is.na(par_fdis1$shape),]$color_point = NA
par_fdis1[is.na(par_fdis1$shape),]$point_alpha = 0.3

par_fdis1$Parameter = gsub("trtdt", "DT", par_fdis1$Parameter)
par_fdis1$Parameter = gsub("trtfd", "FD", par_fdis1$Parameter)
par_fdis1$Parameter = gsub("droughtdrought", "Drought", par_fdis1$Parameter)
par_fdis1$Parameter = gsub("trtdt:droughtdrought", "DT:Drought", par_fdis1$Parameter)
par_fdis1$Parameter = gsub("trtfd:droughtdrought", "FD:Drought", par_fdis1$Parameter)


facs_fdis = c("(Intercept)",
              "DT",
              "FD",
              "Drought",
              "DT:Drought",
              "FD:Drought"
)

# fx_fdis = ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient, group = env)) +
#   geom_point(aes(color = env), alpha = par_fdis$point_alpha, position = position_dodge(width = 0.5), size = 2, show.legend = F) +
#   geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = env), alpha = par_fdis$point_alpha, width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
#   geom_abline(slope = 0, intercept = 0) +
#   coord_flip() + theme_classic() +
#   labs(title = "Functional Dispersion", y = "Effect Size", x = "Predictor", color = "Environment") +
#   #scale_x_discrete(limits = rev(facs_fdis)) +
#   scale_y_continuous(limits = c(-0.55, 0.55))  +
#   guides(alpha = "none")
#par_fdis$Parameter <- factor(par_fdis$Parameter, levels = rev(facs_fdis))
above<-ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient, group = env, shape=env)) +
  geom_point(aes(color = env), alpha = par_fdis$point_alpha, position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = env), alpha = par_fdis$point_alpha, width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic()+
  scale_colour_viridis_d(option="cividis")+#"turbo", direction = -1, begin=0, end = 1)+
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Proportion" = "grey40",
  #   "Forage Quality" = "grey50"))+
  scale_x_discrete(limits = rev(facs_fdis)) +
  labs(y = "Effect Size", x = "Predictor", color = "Function", shape="Function")+
  theme(legend.position = "top",
        legend.text = element_text(size=8),
        legend.title = element_text(size=9))
  

par_sm = standardize_parameters(smmod, method = "refit") %>% mutate(env = "Soil Moisture")
par_nut = standardize_parameters(nutmod, method = "refit") %>% mutate(env = "Soil N")
par_decompgreen = standardize_parameters(decompgreenmod, method = "refit") %>% mutate(env = "Decomposition \n(liable)")
par_decomproo = standardize_parameters(decomproomod, method = "refit") %>% mutate(env = "Decomposition \n(recalcitrant)")
par_sm$Parameter <- par_forage$Parameter
par_nut$Parameter <- par_forage$Parameter
par_decompgreen$Parameter <- par_forage$Parameter
par_decomproo$Parameter <- par_forage$Parameter

par_fdis = rbind(par_sm, par_nut, par_decompgreen, par_decomproo) %>% 
  mutate(shape = ifelse(CI_low <= 0 & CI_high >= 0, NA, "circle"),
        color_line = c(rep("red", 24)),#, rep("blue", 6)),
         color_point = c(rep("red", 24)),#, rep("blue", 6)),
         point_alpha = 1
  )
par_fdis[is.na(par_fdis$shape),]$color_point = NA
par_fdis[is.na(par_fdis$shape),]$point_alpha = 0.3

par_fdis$Parameter = gsub("trtdt", "DT", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd", "FD", par_fdis$Parameter)
par_fdis$Parameter = gsub("droughtdrought", "Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtdt:droughtdrought", "DT:Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd:droughtdrought", "FD:Drought", par_fdis$Parameter)


facs_fdis = c("(Intercept)",
              "DT",
              "FD",
              "Drought",
              "DT:Drought",
              "FD:Drought"
)

below<-ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient, group = env, shape=env)) +
  geom_point(aes(color = env), alpha = par_fdis$point_alpha, position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = env), alpha = par_fdis$point_alpha, width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic()+
  scale_colour_viridis_d(option="cividis")+#"turbo", direction = -1, begin=0, end = 1)+
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Proportion" = "grey40",
  #   "Forage Quality" = "grey50"))+
  scale_x_discrete(limits = rev(facs_fdis)) +
  labs(y = "Effect Size", x = "Predictor", color = "Function", shape="Function")+
  theme(legend.position = "top",
        legend.box="vertical",
        legend.text = element_text(size=8),
        legend.title = element_text(size=9))
# ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient, group = env)) +
#   geom_point(aes(color = env), alpha = par_fdis$point_alpha, position = position_dodge(width = 0.5), size = 2, show.legend = F) +
#   geom_errorbar(aes(ymin = CI_low, ymax = CI_high), alpha = par_fdis$point_alpha, width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
#   geom_abline(slope = 0, intercept = 0) +
#   coord_flip() + theme_classic()+
#   labs(y = "Effect Size", x = "Predictor", color = "Function")
# 


ggarrange(above, below, ncol=1, common.legend = F)
#ggarrange(fx_fdis,
#          fx_fric + theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
#          fx_simpson + theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
#          nrow = 1, ncol = 3, common.legend = T, legend = "bottom", align = "none")
# library(patchwork)
# fx_plot = (above |below + plot_layout(widths = c(1,1))+plot_layout(guides = "collect") & theme(legend.position = "bottom"))
# 
#     

### round 2
par_prod = standardize_parameters(covmod, method = "refit") %>% 
  mutate(env = "Productivity", panel = "Productivity & Invasion")

par_inv = standardize_parameters(invmod, method = "refit") %>% 
  mutate(env = "Invasive Cover", panel = "Productivity & Invasion")

par_cp = standardize_parameters(cpmod, method = "refit") %>% 
  mutate(env = "Crude Protein", panel = "Forage Quality")

par_adf = standardize_parameters(adfmod, method = "refit") %>% 
  mutate(env = "ADF", panel = "Forage Quality")

par_decomp1 = standardize_parameters(decompgreenmod, method = "refit") %>% 
  mutate(env = "Labile", panel = "Decomposition")

par_decomp2 = standardize_parameters(decomproomod, method = "refit") %>% 
  mutate(env = "Recalcitrant", panel = "Decomposition")

par_sm = standardize_parameters(smmod, method = "refit") %>% 
  mutate(env = "Soil Moisture", panel = "Soils")

par_nut = standardize_parameters(nutmod, method = "refit") %>% 
  mutate(env = "Soil N", panel = "Soils")







par_prod = standardize_parameters(covmod, method = "refit") %>% mutate(env = "Productivity")
par_inv = standardize_parameters(invmod, method = "refit") %>% mutate(env = "Invasive Proportion")
par_forage = standardize_parameters(rfvmod, method = "refit") %>% mutate(env = "Forage Quality")
par_prod$Parameter <- par_forage$Parameter
par_inv$Parameter <- par_forage$Parameter
par_sm = standardize_parameters(smmod, method = "refit") %>% mutate(env = "Soil Moisture")
par_nut = standardize_parameters(nutmod, method = "refit") %>% mutate(env = "Soil N")
#par_decompgreen = standardize_parameters(decompgreenmod, method = "refit") %>% mutate(env = "Decomposition \n(liable)")
par_decomproo = standardize_parameters(decomproomod, method = "refit") %>% mutate(env = "Decomposition \n(recalcitrant)")
par_sm$Parameter <- par_forage$Parameter
par_nut$Parameter <- par_forage$Parameter
#par_decompgreen$Parameter <- par_forage$Parameter
par_decomproo$Parameter <- par_forage$Parameter

par_prod <- bind_cols(par_nut, MuMIn::r.squaredGLMM(prodmod)) 
par_inv <- bind_cols(par_nut, MuMIn::r.squaredGLMM(invmod)) 
par_forage <- bind_cols(par_nut, MuMIn::r.squaredGLMM(foragemod)) 
par_sm <- bind_cols(par_nut, MuMIn::r.squaredGLMM(smmod)) 
par_nut <- bind_cols(par_nut, MuMIn::r.squaredGLMM(nutmod)) 
par_decomproo <- bind_cols(par_nut, MuMIn::r.squaredGLMM(decomproomod)) 

par_fdis = rbind(par_prod, par_inv, par_forage,par_sm, par_nut, par_decomproo) %>% 
  mutate(shape = ifelse(CI_low <= 0 & CI_high >= 0, NA, "circle"),
         color_line = c(rep("red", 36)),#, rep("blue", 6)),
         color_point = c(rep("red", 36)),#, rep("blue", 6)),
         point_alpha = 1
  )
par_fdis[is.na(par_fdis$shape),]$color_point = NA
par_fdis[is.na(par_fdis$shape),]$point_alpha = 0.25

par_fdis$Parameter = gsub("trtdt", "DT", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd", "FD", par_fdis$Parameter)
par_fdis$Parameter = gsub("droughtdrought", "Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtdt:droughtdrought", "DT:Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd:droughtdrought", "FD:Drought", par_fdis$Parameter)


facs_fdis = c("(Intercept)",
              "DT",
              "FD",
              "Drought",
              "DT:Drought",
              "FD:Drought"
)

facs_fdis <- facs_fdis[facs_fdis != "(Intercept)"]
par_fdis <- par_fdis %>% filter(Parameter!="(intercept)")

#allsummarys <- bind_rows(par_fdis1, par_fdis)
ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2, show.legend = F) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5), show.legend = F) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic()+
  scale_colour_viridis_d(option="cividis")+#"turbo", direction = -1, begin=0, end = 1)+
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Proportion" = "grey40",
  #   "Forage Quality" = "grey50"))+
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", color = "Function", shape="Function")+
  facet_wrap(~factor(env, levels = c("Productivity", 
                                     "Invasive Proportion", 
                                     "Forage Quality",
                                     "Soil Moisture",
                                     "Soil N",
                                     "Decomposition \n(recalcitrant)")))+#,
                                     #"Decomposition \n(liable)")))+
  theme(guides())
#theme(legend.position = "top",
        # legend.box="vertical",
        # legend.text = element_text(size=8),
        # legend.title = element_text(size=9))
# ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient, group = env)) +
#   geom_point(aes(color = env), alpha = par_fdis$point_alpha, position = position_dodge(width = 0.5), size = 2, show.legend = F) +
#   geom_errorbar(aes(ymin = CI_low, ymax = CI_high), alpha = par_fdis$point_alpha, width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
#   geom_abline(slope = 0, intercept = 0) +
#   coord_flip() + theme_classic()+
#   labs(y = "Effect Size", x = "Predictor", color = "Function")
# 

library(performance)
library(dplyr)


r2_df <- bind_rows(
  MuMIn::r.squaredGLMM(covmod)%>% mutate(env = "Productivity"),
  performance::r(invmod)        %>% mutate(env = "Invasive Proportion"),
  performance::r2(rfvmod)        %>% mutate(env = "Forage Quality"),
  performance::r2(smmod)         %>% mutate(env = "Soil Moisture"),
  performance::r2(nutmod)        %>% mutate(env = "Soil N"),
  performance::r2(decomproomod)  %>% mutate(env = "Decomposition \n(recalcitrant)")
) %>%
  mutate(label = paste0(
    "R²m = ", round(R2_marginal, 2),
    "\nR²c = ", round(R2_conditional, 2)
  ))


ggplot(par_fdis, aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2, show.legend = F) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5), show.legend = F) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic()+
  scale_colour_viridis_d(option="cividis")+#"turbo", direction = -1, begin=0, end = 1)+
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Proportion" = "grey40",
  #   "Forage Quality" = "grey50"))+
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", color = "Function", shape="Function")+
  facet_wrap(~factor(env, levels = c("Productivity", 
                                     "Invasive Proportion", 
                                     "Forage Quality",
                                     "Soil Moisture",
                                     "Soil N",
                                     "Decomposition \n(recalcitrant)")))+#,
  #"Decomposition \n(liable)")))+
  theme(guides()) + geom_text(
  data = r2_df,
  aes(x = Inf, y = Inf, label = label),
  hjust = 1.1, vjust = 1.1,
  inherit.aes = FALSE,
  size = 3
)




#######chat
library(dplyr)
library(ggplot2)
library(parameters)

# --- extract and label models ---
par_prod = standardize_parameters(covmod, method = "refit") %>% 
  mutate(env = "Productivity", panel = "Productivity & Invasion")

par_inv = standardize_parameters(invmod, method = "refit") %>% 
  mutate(env = "Invasive Cover", panel = "Productivity & Invasion")

par_cp = standardize_parameters(cpmod, method = "refit") %>% 
  mutate(env = "Crude Protein", panel = "Forage Quality")

par_adf = standardize_parameters(adfmod, method = "refit") %>% 
  mutate(env = "ADF", panel = "Forage Quality")

par_decomp1 = standardize_parameters(decompgreenmod, method = "refit") %>% 
  mutate(env = "Labile", panel = "Decomposition")

par_decomp2 = standardize_parameters(decomproomod, method = "refit") %>% 
  mutate(env = "Recalcitrant", panel = "Decomposition")

par_sm = standardize_parameters(smmod, method = "refit") %>% 
  mutate(env = "Soil Moisture", panel = "Soils")

par_nut = standardize_parameters(nutmod, method = "refit") %>% 
  mutate(env = "Soil N", panel = "Soils")

par_prod$Parameter <- par_cp$Parameter
par_inv$Parameter <- par_cp$Parameter
par_nut$Parameter <- par_cp$Parameter
par_sm$Parameter <- par_cp$Parameter
par_cp$Parameter <- par_cp$Parameter
par_adf$Parameter <- par_cp$Parameter
par_decomp1$Parameter <- par_cp$Parameter
par_decomp2$Parameter <- par_cp$Parameter

# --- combine ---
par_all = bind_rows(
  par_prod, par_inv,
  par_cp, par_adf,
  par_decomp1, par_decomp2,
  par_sm, par_nut
)
library(tidyr)

# par_all <- par_all %>%
#   complete(panel, env, Parameter)

# --- clean parameter names (your existing logic) ---
par_all$Parameter = gsub("trtdt", "DT", par_all$Parameter)
par_all$Parameter = gsub("trtfd", "FD", par_all$Parameter)
par_all$Parameter = gsub("droughtdrought", "Drought", par_all$Parameter)
par_all$Parameter = gsub("trtdt:droughtdrought", "DT:Drought", par_all$Parameter)
par_all$Parameter = gsub("trtfd:droughtdrought", "FD:Drought", par_all$Parameter)

# --- set factor order ---
facs_fdis = c("DT","FD","Drought","DT:Drought","FD:Drought")
par_all <- par_all %>% 
  filter(Parameter != "(Intercept)", Parameter != "(intercept)") %>%
  mutate(Parameter = factor(Parameter, levels = facs_fdis))

# --- significance formatting ---
par_all <- par_all %>%
  mutate(
    sig = !(CI_low <= 0 & CI_high >= 0),
    point_alpha = ifelse(sig, 1, 0.25)
  )

# --- plot ---
ggplot(par_all, aes(x = Parameter, y = Std_Coefficient, shape = env)) +
  geom_point(aes(alpha = point_alpha),
             position = position_dodge(width = 0.6),
             size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha),
                width = 0.5,
                position = position_dodge(width = 0.6)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() +
  theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Cover" = "grey40",
  #   "Crude Protein" = "#1b9e77",
  #   "ADF" = "#d95f02",
  #   "Labile" = "#7570b3",
  #   "Recalcitrant" = "#e7298a",
  #   "Soil Moisture" = "#66a61e",
  #   "Soil N" = "#e6ab02"
  # )) +
  labs(y = "Effect Size", x = "Predictor", color = "Function") +
  facet_wrap(~panel, ncol = 2)

#
par_comp <- par_all %>% filter(panel=="Productivity & Invasion")
compfig <- ggplot(par_comp, aes(x = Parameter, y = Std_Coefficient, shape = env)) +
  geom_point(aes(alpha = point_alpha),
             position = position_dodge(width = 0.6),
             size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha),
                width = 0.5,
                position = position_dodge(width = 0.6)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() +
  theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Cover" = "grey40",
  #   "Crude Protein" = "#1b9e77",
  #   "ADF" = "#d95f02",
  #   "Labile" = "#7570b3",
  #   "Recalcitrant" = "#e7298a",
  #   "Soil Moisture" = "#66a61e",
  #   "Soil N" = "#e6ab02"
  # )) +
  labs(y = "Effect Size", x = "Predictor", shape = "Function") +
  facet_wrap(~panel, ncol = 2)
par_forage2 <- par_all %>% filter(panel=="Forage Quality")
foragefig <- ggplot(par_forage2, aes(x = Parameter, y = Std_Coefficient, shape = env)) +
  geom_point(aes(alpha = point_alpha),
             position = position_dodge(width = 0.6),
             size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha),
                width = 0.5,
                position = position_dodge(width = 0.6)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() +
  theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Cover" = "grey40",
  #   "Crude Protein" = "#1b9e77",
  #   "ADF" = "#d95f02",
  #   "Labile" = "#7570b3",
  #   "Recalcitrant" = "#e7298a",
  #   "Soil Moisture" = "#66a61e",
  #   "Soil N" = "#e6ab02"
  # )) +
  labs(y = "Effect Size", x = "Predictor", shape = "Function") +
  facet_wrap(~panel, ncol = 2)
par_soil <- par_all %>% filter(panel=="Soils")
ggplot(par_soil, aes(x = Parameter, y = Std_Coefficient, shape = env)) +
  geom_point(aes(alpha = point_alpha),
             position = position_dodge(width = 0.6),
             size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha),
                width = 0.5,
                position = position_dodge(width = 0.6)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() +
  theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  scale_alpha_identity() +
  # scale_color_manual(values = c(
  #   "Productivity" = "black",
  #   "Invasive Cover" = "grey40",
  #   "Crude Protein" = "#1b9e77",
  #   "ADF" = "#d95f02",
  #   "Labile" = "#7570b3",
  #   "Recalcitrant" = "#e7298a",
  #   "Soil Moisture" = "#66a61e",
  #   "Soil N" = "#e6ab02"
  # )) +
  labs(y = "Effect Size", x = "Predictor", shape = "Function") +
  facet_wrap(~panel, ncol = 2)

library(dplyr)
library(ggplot2)
library(ggpubr)  # for ggarrange

# --- combine all models for plotting ---
par_fdis = rbind(
  par_prod, par_inv, 
  par_cp, par_adf,
  par_sm, par_nut,
  par_decomp1, par_decomp2
) %>% 
  mutate(shape = ifelse(CI_low <= 0 & CI_high >= 0, NA, "circle"),
         color_line = "red",
         color_point = "red",
         point_alpha = 1
  )

par_fdis[is.na(par_fdis$shape),]$color_point = NA
par_fdis[is.na(par_fdis$shape),]$point_alpha = 0.25

# --- rename predictors (unchanged) ---
par_fdis$Parameter = gsub("trtdt", "DT", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd", "FD", par_fdis$Parameter)
par_fdis$Parameter = gsub("droughtdrought", "Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtdt:droughtdrought", "DT:Drought", par_fdis$Parameter)
par_fdis$Parameter = gsub("trtfd:droughtdrought", "FD:Drought", par_fdis$Parameter)

facs_fdis = c("DT", "FD", "Drought", "DT:Drought", "FD:Drought")
par_fdis <- par_fdis %>% filter(Parameter %in% facs_fdis)

# --- define function groups ---
forage_plots <- c("Crude Protein", "ADF")
soil_plots <- c("Soil Moisture", "Soil N")
decomp_plots <- c("Labile", "Recalcitrant")
comp_plots <- c("Productivity", "Invasive Cover")

par_fdis <-  merge(par_fdis, r2_df, by=c("panel","env"))

# Replace the factor levels with custom text including R²
par_fdis$env2 <- factor(par_fdis$env, levels = c("Productivity",
                                                "Invasive Cover",
                                                "Crude Protein",
                                                "ADF",
                                                "Soil Moisture",
                                                "Soil N",
                                                "Labile",
                                                "Recalcitrant"),
                       labels = c("Productivity \n R²m = 0.35  R²c = 0.53",
                                  "Invasive Cover \n R²m = 0.24  R²c = 0.47",
                                  "Crude Protein \n R²m = 0.56  R²c = 0.6",
                                  "ADF \n R²m = 0.29  R²c = 0.29",
                                  "Soil Moisture \n R²m = 0.11  R²c = 0.21",
                                  "Soil N \n R²m = 0.73  R²c = 0.85",
                                  "Labile \n R²m = 0.01  R²c = 0.09",
                                  "Recalcitrant \n R²m = 0.19  R²c = 0.21"))

p_comp_data <- p_comp$data %>%
  mutate(letter = case_when(
    env2 == "Productivity" ~ "A",
    env2 == "Invasive Cover" ~ "B",
    env2 == "Crude Protein" ~ "C",
    env2 == "ADF" ~ "D",
    env2 == "Soil Moisture" ~ "E",
    env2 == "Soil N" ~ "F",
    env2 == "Labile" ~ "G",
    env2 == "Recalcitrant" ~ "H"))

# --- make individual 2-panel plots ---
p_forage <- ggplot(par_fdis %>% filter(env %in% forage_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip(clip = "off") + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  #annotate("text", x = .1, y = .9, label = "A         B", size = 4) +  # adjust x/y per facet
  facet_wrap(~env2, ncol = 2) +
  scale_alpha_identity() +
  geom_text(x=6, y=-.9,label = p_comp_data$letter, 
           inherit.aes = FALSE )+
  # geom_text(data = r2_df%>% filter(env %in% forage_plots), aes(x = .8, y = .1, label = label, ), 
  #           inherit.aes = T, size = 6) +
  #annotate("text", x = 4.5, y = 0.9, label = "B", size = 6) +   # second facet
  labs(y = "Effect Size", x = "Predictor", title = "Forage Quality \n")

p_soil <- ggplot(par_fdis %>% filter(env %in% soil_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env2, ncol = 2)+
  scale_alpha_identity() +
 # geom_text(data = letters_df, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 6) +
  labs(y = "Effect Size", x = "Predictor", title = "Soils \n")

p_decomp1 <- ggplot(par_fdis %>% filter(env %in% decomp_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env2, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Decomposition \n")+
  theme(guides(legend="none"))


p_comp <- ggplot(par_fdis %>% filter(env %in% comp_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env2, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Composition \n")

# --- combine all four 2-panel plots into 4x2 grid ---
ggarrange(p_comp, p_forage, p_soil, p_decomp1,
          ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))


##redo:
trtsumfig<-  ggarrange(
  annotate_figure(ggarrange(par_fdis[par_fdis$Parameter =='Producivity'],
                            par_fdis[par_fdis$Parameter =='Invasive Cover'],
                            # cptrtplot, adftrtplot,
                            # smtrtplot, nuttrtplot, 
                            # decompgreenmod, decomproomod, 
                            common.legend = T,
                            labels = c("A", "B"), 
                            vjust = -.03,
                            legend="none"), 
                  top = "Composition \n ")
  annotate_figure(ggarrange(cpldmcplot, cpfullplot,
                            adfldmcplot, adffullplot, common.legend = T,
                            labels = c("E", "F", "G", "H"), 
                            vjust = -.03,
                            legend = "none"), 
                  top = "Forage Quality \n "),
  annotate_figure(ggarrange(smldmcplot, smfullplot,
                            nutldmcplot, nutfullplot, common.legend = T,
                            labels = c("I", "J", "K", "L"), 
                            vjust = -.05,
                            legend = "none"), 
                  top = "Soils \n "),
  annotate_figure(ggarrange(rooldmcplot, roofullplot,
                            greenldmcplot, greenfullplot,common.legend = T,
                            labels = c("M", "N", "O", "P"), 
                            vjust = -.03,
                            legend = "none"), 
                  top = "Decomposition \n"),
  common.legend = T,
  ncol = 2, nrow=2) #,labels = c("A", "B", "C", "D"))
sharedlegend<-cowplot::get_legend(covldmcplot+theme(legend.position = "bottom"))

allcomboplot <- ggarrange(combinedfig, sharedlegend, ncol=1, heights = c(2,.15)) 
allcomboplot


# 
# 
# ########### COMBINED CWM/ FD plots
# combinedfig<- ggarrange(
#   annotate_figure(ggarrange(covldmcplot, covfullplot,
#                             invldmcplot, invfullplot, common.legend = T,
#                             labels = c("A", "B", "C","D"), 
#                             vjust = -.03,
#                             legend="none"), 
#                   top = "Composition \n "),
#   annotate_figure(ggarrange(cpldmcplot, cpfullplot,
#                             adfldmcplot, adffullplot, common.legend = T,
#                             labels = c("E", "F", "G", "H"), 
#                             vjust = -.03,
#                             legend = "none"), 
#                   top = "Forage Quality \n "),
#   annotate_figure(ggarrange(smldmcplot, smfullplot,
#                             nutldmcplot, nutfullplot, common.legend = T,
#                             labels = c("I", "J", "K", "L"), 
#                             vjust = -.05,
#                             legend = "none"), 
#                   top = "Soils \n "),
#   annotate_figure(ggarrange(rooldmcplot, roofullplot,
#                             greenldmcplot, greenfullplot,common.legend = T,
#                             labels = c("M", "N", "O", "P"), 
#                             vjust = -.03,
#                             legend = "none"), 
#                   top = "Decomposition \n"),
#           common.legend = T,
#           ncol = 2, nrow=2) #,labels = c("A", "B", "C", "D"))
# sharedlegend<-cowplot::get_legend(covldmcplot+theme(legend.position = "bottom"))
# 
# allcomboplot <- ggarrange(combinedfig, sharedlegend, ncol=1, heights = c(2,.15)) 
# allcomboplot
# #top = "Decomposition")
# # ggarrange(covldmcplot, covrdfdplot, covfullplot,
# #           invldmcplot, invrdfdplot, invfullplot,
# #           cpldmcplot, cpfdrdplot, cpfullplot,
# #           adfldmcplot, adffdrdplot, adffullplot,
# #           nutldmcplot, nutfdrdplot, nutfullplot,
# #           smldmcplot, smfdrdplot, smfullplot,
# #           rooldmcplot, roofdrdplot, roofullplot,
# #           greenldmcplot, greenfdrdplot, greenfullplot,
# #           common.legend = T,
# #           ncol = 3, nrow=4) #,labels = c("A", "B", "C", "D"))
# 
# ###### working enough!
# library(ggplot2)
# library(dplyr)
# library(ggpubr)
# 
# # --- function to make ONE panel (keeps your env2 label) ---
# make_single_plot <- function(df, this_env) {
#   ggplot(df %>% filter(env == this_env),
#          aes(x = Parameter, y = Std_Coefficient)) +
#     geom_point(aes(alpha = point_alpha),
#                position = position_dodge(width = 0.5), size = 2) +
#     geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha),
#                   width = 0.5, linewidth = 1,
#                   position = position_dodge(width = 0.5)) +
#     geom_abline(slope = 0, intercept = 0) +
#     coord_flip() +
#     theme_classic() +
#     scale_x_discrete(limits = rev(facs_fdis)) +
#     facet_wrap(~env2, ncol = 1) +   # keeps your R² labels
#     scale_alpha_identity() +
#     theme()
# }
# 
# # --- build all 8 plots ---
# p_prod <- make_single_plot(par_fdis, "Productivity")+
#   theme(axis.ticks.x = element_blank())
# p_inv  <- make_single_plot(par_fdis, "Invasive Cover")
# 
# p_cp   <- make_single_plot(par_fdis, "Crude Protein")
# p_adf  <- make_single_plot(par_fdis, "ADF")
# 
# p_sm   <- make_single_plot(par_fdis, "Soil Moisture")
# p_nut  <- make_single_plot(par_fdis, "Soil N")
# 
# p_lab  <- make_single_plot(par_fdis, "Labile")
# p_rec  <- make_single_plot(par_fdis, "Recalcitrant")
# 
# # --- combine into final figure ---
# trtsumfig <- ggarrange(
#   
#   annotate_figure(
#     ggarrange(p_prod, p_inv,
#               labels = c("A", "B"),
#               ncol = 2,
#               common.legend = TRUE,
#               legend = "none"),
#     top = "Composition"
#   ),
#   
#   annotate_figure(
#     ggarrange(p_cp, p_adf,
#               labels = c("C", "D"),
#               ncol = 2,
#               common.legend = TRUE,
#               legend = "none"),
#     top = "Forage Quality"
#   ),
#   
#   annotate_figure(
#     ggarrange(p_sm, p_nut,
#               labels = c("E", "F"),
#               ncol = 2,
#               common.legend = TRUE,
#               legend = "none"),
#     top = "Soils"
#   ),
#   
#   annotate_figure(
#     ggarrange(p_lab, p_rec,
#               labels = c("G", "H"),
#               ncol = 2,
#               common.legend = TRUE,
#               legend = "none"),
#     top = "Decomposition"
#   ),
#   
#   ncol = 2, nrow = 2,
#   common.legend = TRUE
# )
# 
# trtsumfig




##### for appendix boxplot
ggarrange(covtrtplot, invtrtplot, 
          cptrtplot, adftrtpplot,
          smtrtplot, m14,
          decompgplot, decomprplot,
          common.legend = T,
          nrow=4, ncol=2)


#### for model summary outputs
#####output of models as formatted tables for supplemental:
clean_terms <- function(term) {
  term %>%
    gsub("\\(Intercept\\)", "Intercept", .) %>%
    gsub("trt", "Seeding treatment ", .) %>%
    gsub("drought2", "Precipitation reduction ", .)%>%
    gsub("drought2:trt", "Interaction ", .) }


library(modelsummary)

modelsummary(
  c(covmod_ldmc, covmod_full, 
    invmod_ldmc, invmod_full,
    cpmod_ldmc, cpmod_full,
    adfmod_ldmc, adfmod_full,
    smmod_ldmc, smmod_full,
    nutmod_ldmc, nutmod_full,
    deccompgmod_ldmc, deccompgmod_full,
    deccomprmod_ldmc, deccomprmod_full),
  stars = TRUE,
  output = "model_table.docx"
)

models <- list(
  "Productivity (LDMC)" = covmod_ldmc,
  "Productivity (Full)" = covmod_full,
  "Proportion Invasive (LDMC)"  = invmod_ldmc,
  "Proportion Invasive  (Full)"  = invmod_full,
  "Crude Protien (LDMC)"   = cpmod_ldmc,
  "Crude Protien (Full)"   = cpmod_full,
  "ADF (LDMC)"  = adfmod_ldmc,
  "ADF (Full)"  = adfmod_full,
  "Soil Moisture (LDMC)" = smmod_ldmc,
  "Soil Moisture (Full)" = smmod_full,
  "Soil N (LDMC)"      = nutmod_ldmc,
  "Soil N (Full)"      = nutmod_full,
  "Decomposition-laible (LDMC)" = deccompgmod_ldmc,
  "Decomposition-laible G (Full)" = deccompgmod_full,
  "Decomposition-decalcitrant R (LDMC)" = deccomprmod_ldmc,
  "Decomposition-decalcitrant (Full)" = deccomprmod_full
)

modelsummary(models, stars = TRUE, output = "model_table.docx")

library(modelsummary)

model_list <- list(
  Productivity = list(covmod_ldmc, covmod_full),
  Invasion     = list(invmod_ldmc, invmod_full),
  CrudeProtein = list(cpmod_ldmc, cpmod_full),
  ADF          = list(adfmod_ldmc, adfmod_full),
  SoilMoisture = list(smmod_ldmc, smmod_full),
  Nitrogen     = list(nutmod_ldmc, nutmod_full),
  DecompG      = list(deccompgmod_ldmc, deccompgmod_full),
  DecompR      = list(deccomprmod_ldmc, deccomprmod_full)
)

for (name in names(model_list)) {
  modelsummary(
    model_list[[name]],
    stars = TRUE,
    output = paste0("table_", name, ".docx"),
    title = paste("Model results:", name)
  )

tab <- tab |>
  autofit() |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  #fontsize(size = 10, part = "all")

#save(tab, path = paste0("table_", name, ".docx"))
}




library(broom.mixed)
library(knitr)
library(kableExtra)

model_summary <- tidy(covldmcmod)

# Separate fixed and random effects
fixed_effects <- model_summary %>% filter(effect == "fixed")
random_effects <- model_summary %>% filter(effect == "ran_pars")

# Adjusting Terms to be More Descriptive
fixed_effects <- fixed_effects %>%
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `trt1` = "Treatment 1",
                       `trt2` = "Treatment 2",
                       `trt3` = "Treatment 3",
                       `drought1` = "Drought Level 1",
                       `drought2` = "Drought Level 2",
                       `year1` = "Year 1",
                       `year2` = "Year 2"))

random_effects <- random_effects %>%
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept"))

# Combine fixed and random effects with proper column names
fixed_effects <- fixed_effects %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(effect_type = "Fixed Effect")

random_effects <- random_effects %>%
  select(term, estimate, std.error) %>%
  mutate(statistic = NA, p.value = NA, effect_type = "Random Effect")

# Rename columns to be more descriptive
colnames(fixed_effects) <- c("Term", "Estimate", "Std. Error", "Statistic", "P-value", "Effect Type")
colnames(random_effects) <- c("Term", "Estimate", "Std. Error", "Statistic", "P-value", "Effect Type")

# Combine both into one dataframe
combined_effects <- bind_rows(fixed_effects, random_effects)

# Create the table
table<-kable(combined_effects, caption = "Summary of Linear Mixed-Effects Model", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F, position = "left")

# Save the table as HTML
save_kable(table, "model_summaryirca.doc")




#####
library(modelsummary)

# -------------------------------
# Clean coefficient names
# -------------------------------
coef_map <- c(
  "(Intercept)" = "Intercept",
  "LDMC"        = "CWM LDMC",
  "Precipitation"    = "Reduction"
)

# -------------------------------
# Limit goodness-of-fit stats
# -------------------------------
gof_map <- data.frame(
  raw   = c("nobs", "AIC"),
  clean = c("N", "AIC"),
  fmt   = c(0, 1)
)

# -------------------------------
# Productivity model: LDMC only
# -------------------------------
modelsummary(
  covmod_ldmc,               # your model
  stars    = TRUE,
  coef_map = coef_map,
  gof_map  = gof_map,
  fmt      = 3,
  output   = "table_Productivity_LDMC.docx",
  title    = "Model results: Productivity (LDMC)"
)

# -------------------------------
# Productivity model: Full model
# -------------------------------
modelsummary(
  covmod_full,               # your model
  stars    = TRUE,
  coef_map = coef_map,
  gof_map  = gof_map,
  fmt      = 3,
  output   = "table_Productivity_Full.docx",
  title    = "Model results: Productivity (Full)"
)




# -------------------------------
# Clean coefficient names
# -------------------------------
coef_map <- c(
  "(Intercept)" = "Intercept",
  "LDMC"        = "CWM LDMC",
  "trt1"        = "Treatment 1",
  "trt2"        = "Treatment 2",
  "trt3"        = "Treatment 3",
  "drought1"    = "Drought Level 1",
  "drought2"    = "Drought Level 2",
  "year1"       = "Year 1",
  "year2"       = "Year 2"
)

# -------------------------------
# Limit goodness-of-fit stats
# -------------------------------
gof_map <- data.frame(
  raw   = c("nobs", "AIC"),
  clean = c("N", "AIC"),
  fmt   = c(0, 1)
)

# -------------------------------
# Organize models by ecosystem function
# -------------------------------
model_list <- list(
  Productivity = list("LDMC" = covmod_ldmc, "Full" = covmod_full),
  Invasion     = list("LDMC" = invmod_ldmc, "Full" = invmod_full),
  CrudeProtein = list("LDMC" = cpmod_ldmc, "Full" = cpmod_full),
  ADF          = list("LDMC" = adfmod_ldmc, "Full" = adfmod_full),
  SoilMoisture = list("LDMC" = smmod_ldmc, "Full" = smmod_full),
  Nitrogen     = list("LDMC" = nutmod_ldmc, "Full" = nutmod_full),
  DecompG      = list("LDMC" = deccompgmod_ldmc, "Full" = deccompgmod_full),
  DecompR      = list("LDMC" = deccomprmod_ldmc, "Full" = deccomprmod_full)
)

# -------------------------------
# Loop over each ecosystem function and save Word table
# -------------------------------
for (name in names(model_list)) {
  
  # Combine LDMC + Full models in one table
  tab <- modelsummary(
    model_list[[name]],
    stars    = TRUE,
    coef_map = coef_map,
    gof_map  = gof_map,
    fmt      = 3,
    output   = "model_table.docx",
    title    = paste("Model results:", name)
  )
  
  # Overwrite-safe file naming
  file_name <- paste0("table_", name, ".docx")
  if (file.exists(file_name)) file.remove(file_name)
  
  # Save table
  modelsummary(
    model_list[[name]],
    stars    = TRUE,
    coef_map = coef_map,
    gof_map  = gof_map,
    fmt      = 3,
    output   = file_name,
    title    = paste("Model results:", name)
  )
  
  message("Saved table for: ", name)
}



### actually save table?
library(glmmTMB)
library(broom.mixed)
library(flextable)
library(officer)
library(dplyr)

# -------------------------------
# Replace this with your model
# -------------------------------
# model <- glmmTMB(Response ~ Size + SRL * SpringRain * IntraDensity + 
#                  (1|Block) + (Size + IntraDensity | Species), 
#                  data = mydata, family = gaussian)

# -------------------------------
# Extract fixed effects
# -------------------------------
fixed <- tidy(covmod_ldmc, effects = "fixed") %>%
  dplyr::select(term, estimate, std.error, statistic, p.value)

# -------------------------------
# Extract random effects
# -------------------------------
random <- summary(covmod_ldmc)$varcor
  tidy(covmod_ldmc, effects = "ran_pars") %>%
  dplyr::select(term, estimate, std.error) %>%
  rename(Variance = estimate, `Std. Dev` = std)

# -------------------------------
# Build flextables
# -------------------------------
ft_fixed <- flextable(fixed) %>%
  set_header_labels(
    term = "Fixed effects",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "z-value",
    p.value = "P-value"
  ) %>%
  colformat_double(digits = 3) %>%
  autofit() %>%
  theme_box()

ft_random <- flextable(random) %>%
  set_header_labels(
    term = "Random effects",
    Variance = "Variance",
    `Std. Dev` = "Std. Dev"
  ) %>%
  colformat_double(digits = 3) %>%
  autofit() %>%
  theme_box()

# -------------------------------
# Save to Word
# -------------------------------
doc <- read_docx() %>%
  body_add_par("Model results", style = "heading 1") %>%
  body_add_flextable(ft_fixed) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_random)

print(doc, target = "glmmTMB_model_results.docx")



# Get summary object
sm <- summary(covmod_ldmc)

# Extract random effects
rand_eff <- sm$varcor   # this is a list for each grouping factor

# Initialize empty data.frame
rand_table <- data.frame()

for (grp in names(rand_eff)) {
  tmp <- as.data.frame(rand_eff[[grp]])
  tmp$Group <- grp
  rand_table <- rbind(rand_table, tmp)
}

# Add residuals
resid_table <- data.frame(
  Group = "Residual",
  Name = NA,
  Variance = sm$sigma^2,
  `Std.Dev.` = sm$sigma
)

rand_table <- rbind(rand_table, resid_table)

# Clean up column names
rand_table <- rand_table %>%
  select(Group, Name = rownames(.), Variance, `Std.Dev.`)

# Check output
print(rand_table)



