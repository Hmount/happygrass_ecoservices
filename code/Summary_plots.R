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