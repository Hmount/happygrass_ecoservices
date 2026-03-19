#### Find community weighted mean traits in each plot in each year 
#### (differs slightly from previous analysis because weed-y volunteer species
#### are also included (should they be?))

## packages
library(tidyverse)
#normalize FD to compare and plot
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## load data
# community composition data
comp <- read.csv("data/communities/comp_wy_plot.csv")# community composition per subplot
comp <- comp %>% filter(year == "2023") #keep only 2023 data 
comp$drought <- as.factor(comp$drought)
comp$year<-as.factor(comp$year)
comp$trt <- as.factor(comp$trt)
comp$block <- as.factor(comp$block)
groupingvars <- comp[,c("block","trt","drought")]
comp <- comp %>% unite(plot, c(block, trt), sep = "_", remove=F) # make unique plot variable

# previous CWMs with just natives (not using)
#cwms <- read.csv("data/communities/cwm_wy.csv")

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

#prep matrix and order for functcomp()
rownames(traits) <- NULL
traitmatrix <- traits %>% column_to_rownames("spcode")
traitmatrix <- traitmatrix[order(rownames(traitmatrix)),]
traitmatrix <- traitmatrix[,2:9]

# Reshape community data into a matrix and arrange alphabetically
compmatrix <- comp %>% column_to_rownames("plot") 
compmatrix <- compmatrix[4:60]
selectspecies <- unique(traits$spcode) #make list of species with trait data
compmatrix <- compmatrix %>% select(all_of(selectspecies)) #ONLY species with trait data
compmatrix <- compmatrix[,order(colnames(compmatrix))] #order
compmatrix <- replace(compmatrix, is.na(compmatrix), 0) #replace NA with 0 for communities


## Calculate community weighted means. Note that bin.num must be specified for binary outcomes
cwms <- FD::functcomp(as.matrix(traitmatrix), as.matrix(compmatrix), bin.num=c("graminoid"))

#get ID columns back
cwms$block <- factor(groupingvars$block)
cwms$trt <- factor(groupingvars$trt)
cwms$drought <- factor(groupingvars$drought)


rel_cover <- comp[,7:62] %>%
  mutate(total_cover = rowSums(., na.rm = TRUE)) %>%
  mutate(across(-total_cover, ~ .x / total_cover)) %>%
  select(-total_cover)
rel_cover_subset <- rel_cover %>%
  select(all_of(selectspecies))
# Calculate total relative cover in the subset
cover_retained <- rel_cover_subset %>%
  mutate(relative_cover_sum = rowSums(., na.rm = TRUE))
cover_retained$block <- comp$block
cover_retained$trt <- comp$trt
cover_retained$block <- comp$plot


dropped_plots <- cover_retained %>%
  filter(relative_cover_sum < 0.8)

# remove plots with invalid CWMs (below 80% cover has traits)
cwmssub <- cwms[!rownames(cwms) %in% c("32_r", "32_ir"), ]

library(ggordiplots)
#nonmetric multidimensional scaling and ploting of ellipses by treatment
test <- vegan::metaMDS(cwmssub[,1:8], distance="euclidean")
test
plot(test, main="euclidean") #type = t, display = sites
ordiellipse(test, cwmssub$trt, col=c("orange","green","red","darkgray"), conf=0.95)
vectors <- envfit(test,cwmssub[,1:8])
plot(vectors,p.max=0.05)


write.csv(cwmssub, "data/communities/validCWM23.csv")

#Calculating functional diversity of each trait and extracting RaoQ (RoaQ is not scaled here, needs to be done when using these columns)
compmatrix <- compmatrix %>% select(-ARPU)
traitmatrix <- traitmatrix[-7,] #remove ARPU
compmatrix <- compmatrix %>%
  select(which(colSums(compmatrix) > 0))
row_totals <- rowSums(compmatrix, na.rm = TRUE)
compmatrix <- compmatrix[row_totals > 0, , drop = FALSE]
traitmatrix <- traitmatrix[colnames(compmatrix), , drop = FALSE]#keep only if found in communities

raoq <- FD::dbFD(as.matrix(scale(traitmatrix)), as.matrix(compmatrix),  corr = "cailliez")
leafn <- FD::dbFD(as.matrix(traitmatrix[,"leafn", drop=F]), as.matrix(compmatrix))
#lop <- FD::dbFD(as.matrix(trait.matrix.wy[,"lop"]), as.matrix(compmatrix))
ldmc <- FD::dbFD(as.matrix(traitmatrix[,"ldmc", drop=F]), as.matrix(compmatrix))
srl <- FD::dbFD(as.matrix(traitmatrix[,"srl", drop=F]), as.matrix(compmatrix))
rootdiam <- FD::dbFD(as.matrix(traitmatrix[,"rootdiam", drop=F]), as.matrix(compmatrix))
#veg <- FD::dbFD(as.matrix(traitmatrix[,"veg", drop=F]), as.matrix(compmatrix))
cwm_roaq23 <- data.frame(leafn=normalize(leafn$RaoQ),
                         ldmc=normalize(ldmc$RaoQ),
                         srl=normalize(srl$RaoQ),
                         rootdiam=normalize(rootdiam$RaoQ),
                         full=normalize(raoq$RaoQ))
cwm_roaq23 <- cwm_roaq23 %>% rownames_to_column("plot")
plotnames <- cwmssub %>% rownames_to_column("plot")
cwm_roaq23 <- merge(cwm_roaq23,plotnames[,c(1,10:12)], by="plot")
cwm_roaq23$trt <- factor(cwm_roaq23$trt, levels = c('ir','dt','fd','r'),ordered = TRUE)

write.csv(cwm_roaq23, "data/communities/FD23.csv")









#### not using first year (no differences)
# ####also for 2021
# comp <- read.csv("data/communities/comp_wy_plot.csv")# community composition per subplot
# comp <- comp %>% filter(year == "2021") #keep only 2023 data 
# comp$drought <- as.factor(comp$drought)
# comp$year<-as.factor(comp$year)
# comp$trt <- as.factor(comp$trt)
# comp$block <- as.factor(comp$block)
# groupingvars <- comp[,c("block","trt","drought")]
# comp <- comp %>% unite(plot, c(block, trt), sep = "_", remove=T) # make unique plot variable
# 
# # previous CWMs with just natives (not using)
# #cwms <- read.csv("data/communities/cwm_wy.csv")
# 
# # trait data (see trait_wrangling.R)
# traits <- read.csv("data/traits/mastertrait.csv")
# 
# #scale traits
# traits$srl = scale(log(traits$srl))
# traits$ldmc = scale(log(traits$ldmc))
# traits$leafn = scale(log(traits$leafn))
# #traits$lop = scale(traits$lop)
# traits$rootdiam = scale(log(traits$rootdiam))
# traits$sla = scale(log(traits$sla))
# traits$rdmc = scale(log(traits$rdmc))
# 
# #prep matrix and order for functcomp()
# rownames(traits) <- NULL
# traitmatrix <- traits %>% column_to_rownames("spcode")
# traitmatrix <- traitmatrix[order(rownames(traitmatrix)),]
# traitmatrix <- traitmatrix[,2:9]
# 
# # Reshape community data into a matrix and arrange alphabetically
# compmatrix <- comp %>% column_to_rownames("plot") 
# compmatrix <- compmatrix[4:63]
# selectspecies <- unique(traits$spcode) #make list of species with trait data
# compmatrix <- compmatrix %>% select(all_of(selectspecies)) #ONLY species with trait data
# compmatrix <- compmatrix[,order(colnames(compmatrix))] #order
# compmatrix <- replace(compmatrix, is.na(compmatrix), 0) #replace NA with 0 for communities
# 
# 
# ## Calculate community weighted means. Note that bin.num must be specified for binary outcomes
# cwms <- FD::functcomp(as.matrix(traitmatrix), as.matrix(compmatrix), bin.num=c("graminoid"))
# 
# #get ID columns back
# cwms$block <- factor(groupingvars$block)
# cwms$trt <- factor(groupingvars$trt)
# cwms$drought <- factor(groupingvars$drought)
# 
# # # remove communities that do not have traits for at least 80% of relative abundance
# # NO invasion, none to drop
# 
# #nonmetric multidimensional scaling and ploting of ellipses by treatment
# test <- vegan::metaMDS(cwmssub[,1:8], distance="euclidean")
# test
# plot(test, main="euclidean") #type = t, display = sites
# ordiellipse(test, cwmssub$trt, col=c("orange","green","red","darkgray"), conf=0.95)
# vectors <- envfit(test,cwmssub[,1:8])
# plot(vectors,p.max=0.05)
# 
# 
# write.csv(cwms, "data/communities/validCWM21.csv")
# 
# #Calculating functional diversity of each trait and extracting RaoQ (RoaQ is not scaled here, needs to be done when using these columns)
# compmatrix <- compmatrix %>%
#   select(which(colSums(compmatrix) > 0))
# traitmatrix <- traitmatrix[colnames(compmatrix), , drop = FALSE]#keep only if found in communities
# raoq <- FD::dbFD(as.matrix(scale(traitmatrix)), as.matrix(compmatrix),  corr = "cailliez")
# leafn <- FD::dbFD(as.matrix(traitmatrix[,"leafn", drop=F]), as.matrix(compmatrix))
# #lop <- FD::dbFD(as.matrix(trait.matrix.wy[,"lop"]), as.matrix(compmatrix))
# ldmc <- FD::dbFD(as.matrix(traitmatrix[,"ldmc", drop=F]), as.matrix(compmatrix))
# srl <- FD::dbFD(as.matrix(traitmatrix[,"srl", drop=F]), as.matrix(compmatrix))
# rootdiam <- FD::dbFD(as.matrix(traitmatrix[,"rootdiam", drop=F]), as.matrix(compmatrix))
# #veg <- FD::dbFD(as.matrix(traitmatrix[,"veg", drop=F]), as.matrix(compmatrix))
# cwm_roaq21 <- data.frame(leafn=leafn$RaoQ,
#                          ldmc=ldmc$RaoQ,
#                          srl=srl$RaoQ,
#                          rootdiam=rootdiam$RaoQ,
#                          full=raoq$RaoQ)
# cwm_roaq21 <- cwm_roaq21 %>% rownames_to_column("plot")
# plotnames <- cwmssub %>% rownames_to_column("plot")
# cwm_roaq21 <- merge(cwm_roaq21,plotnames[,c(1,9:12)], by="plot")
# cwm_roaq21$trt <- factor(cwm_roaq21$trt, levels = c('ir','dt','fd','rand'),ordered = TRUE)
# 
# write.csv(cwm_roaq21, "data/communities/FD21.csv")
# 




####also for 2022 (soil moisture)
comp <- read.csv("data/communities/comp_wy_plot.csv")# community composition per subplot
comp <- comp %>% filter(year == "2022") 
comp$drought <- as.factor(comp$drought)
comp$year<-as.factor(comp$year)
comp$trt <- as.factor(comp$trt)
comp$block <- as.factor(comp$block)
groupingvars <- comp[,c("block","trt","drought")]
comp <- comp %>% unite(plot, c(block, trt), sep = "_", remove=T) # make unique plot variable

# previous CWMs with just natives (not using)
#cwms <- read.csv("data/communities/cwm_wy.csv")

# trait data (see trait_wrangling.R)
traits <- read.csv("data/traits/mastertrait.csv")

#scale traits
# traits$srl = scale(log(traits$srl))
# traits$ldmc = scale(log(traits$ldmc))
# traits$leafn = scale(log(traits$leafn))
# #traits$lop = scale(traits$lop)
# traits$rootdiam = scale(log(traits$rootdiam))
# traits$sla = scale(log(traits$sla))
# traits$rdmc = scale(log(traits$rdmc))

#prep matrix and order for functcomp()
rownames(traits) <- NULL
traitmatrix <- traits %>% column_to_rownames("spcode")
traitmatrix <- traitmatrix[order(rownames(traitmatrix)),]
traitmatrix <- traitmatrix[,2:9]

# Reshape community data into a matrix and arrange alphabetically
compmatrix <- comp %>% column_to_rownames("plot") 
compmatrix <- compmatrix[4:63]
selectspecies <- unique(traits$spcode) #make list of species with trait data
compmatrix <- compmatrix %>% select(all_of(selectspecies)) #ONLY species with trait data
compmatrix <- compmatrix[,order(colnames(compmatrix))] #order
compmatrix <- replace(compmatrix, is.na(compmatrix), 0) #replace NA with 0 for communities


## Calculate community weighted means. Note that bin.num must be specified for binary outcomes
cwms <- FD::functcomp(as.matrix(traitmatrix), as.matrix(compmatrix), bin.num=c("graminoid"))

#get ID columns back
cwms$block <- factor(groupingvars$block)
cwms$trt <- factor(groupingvars$trt)
cwms$drought <- factor(groupingvars$drought)

rel_cover <- comp[,5:60] %>%
  mutate(total_cover = rowSums(., na.rm = TRUE)) %>%
  mutate(across(-total_cover, ~ .x / total_cover)) %>%
  select(-total_cover)
rel_cover_subset <- rel_cover %>%
  select(all_of(selectspecies))
# Calculate total relative cover in the subset
cover_retained <- rel_cover_subset %>%
  mutate(relative_cover_sum = rowSums(., na.rm = TRUE))
cover_retained$block <- comp$block
cover_retained$trt <- comp$trt
cover_retained$block <- comp$plot


dropped_plots <- cover_retained %>%
  filter(relative_cover_sum < 0.8)

# remove plots with invalid CWMs (below 80% cover has traits)
cwmssub <- cwms[!rownames(cwms) %in% c("12_ir", "18_ir", "40_ir", "45_dt"), ]

#nonmetric multidimensional scaling and ploting of ellipses by treatment
test <- vegan::metaMDS(cwmssub[,1:8], distance="euclidean")
test
plot(test, main="euclidean") #type = t, display = sites
ordiellipse(test, cwmssub$trt, col=c("orange","green","red","darkgray"), conf=0.95)
vectors <- envfit(test,cwmssub[,1:8])
plot(vectors,p.max=0.05)


write.csv(cwmssub, "data/communities/validCWM22.csv")


#Calculating functional diversity of each trait and extracting RaoQ (RoaQ is not scaled here, needs to be done when using these columns)
compmatrix <- compmatrix %>%
  select(which(colSums(compmatrix) > 0))
row_totals <- rowSums(compmatrix, na.rm = TRUE)
compmatrix <- compmatrix[row_totals > 0, , drop = FALSE]
traitmatrix <- traitmatrix[colnames(compmatrix), , drop = FALSE]#keep only if found in communities

raoq <- FD::dbFD(as.matrix(scale(traitmatrix)), as.matrix(compmatrix),  corr = "cailliez")
leafn <- FD::dbFD(as.matrix(traitmatrix[,"leafn", drop=F]), as.matrix(compmatrix))
#lop <- FD::dbFD(as.matrix(trait.matrix.wy[,"lop"]), as.matrix(compmatrix))
ldmc <- FD::dbFD(as.matrix(traitmatrix[,"ldmc", drop=F]), as.matrix(compmatrix))
srl <- FD::dbFD(as.matrix(traitmatrix[,"srl", drop=F]), as.matrix(compmatrix))
rootdiam <- FD::dbFD(as.matrix(traitmatrix[,"rootdiam", drop=F]), as.matrix(compmatrix))
#veg <- FD::dbFD(as.matrix(traitmatrix[,"veg", drop=F]), as.matrix(compmatrix))
cwm_roaq22 <- data.frame(leafn=normalize(leafn$RaoQ),
                         ldmc=normalize(ldmc$RaoQ),
                         srl=normalize(srl$RaoQ),
                         rootdiam=normalize(rootdiam$RaoQ),
                         full=normalize(raoq$RaoQ))
cwm_roaq22 <- cwm_roaq22 %>% rownames_to_column("plot")
plotnames <- cwmssub %>% rownames_to_column("plot")
cwm_roaq22 <- merge(cwm_roaq22,plotnames[,c(1,10:12)], by="plot")
cwm_roaq22$trt <- factor(cwm_roaq22$trt, levels = c('ir','dt','fd','r'),ordered = TRUE)

write.csv(cwm_roaq22, "data/communities/FD22.csv")



