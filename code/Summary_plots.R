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
  MuMln::r.squaredGLMM(covmod)%>% mutate(env = "Productivity"),
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

# --- make individual 2-panel plots ---
p_forage <- ggplot(par_fdis %>% filter(env %in% forage_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Forage Quality")

p_soil <- ggplot(par_fdis %>% filter(env %in% soil_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Soils")

p_decomp <- ggplot(par_fdis %>% filter(env %in% decomp_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Decomposition")

p_comp <- ggplot(par_fdis %>% filter(env %in% comp_plots), aes(x = Parameter, y = Std_Coefficient)) +
  geom_point(aes(alpha = point_alpha), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, alpha = point_alpha), width = 0.5, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 0, intercept = 0) +
  coord_flip() + theme_classic() +
  geom_text(aes(label = label))+
  scale_x_discrete(limits = rev(facs_fdis)) +
  facet_wrap(~env, ncol = 2) +
  scale_alpha_identity() +
  labs(y = "Effect Size", x = "Predictor", title = "Composition")

# --- combine all four 2-panel plots into 4x2 grid ---
ggarrange(p_comp, p_forage, p_soil, p_decomp,
          ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))



########### COMBINED CWM/ FD plots
ggarrange(covldmcplot, covrdfdplot, covfullplot,
          invldmcplot, invrdfdplot, invfullplot,
          cpldmcplot, cpfdrdplot, cpfullplot,
          adfldmcplot, adffdrdplot, adffullplot,
          nutldmcplot, nutfdrdplot, nutfullplot,
          smldmcplot, smfdrdplot, smfullplot,
          rooldmcplot, roofdrdplot, roofullplot,
          greenldmcplot, greenfdrdplot, greenfullplot,
          common.legend = T,
          ncol = 3, nrow=4) #,labels = c("A", "B", "C", "D"))

