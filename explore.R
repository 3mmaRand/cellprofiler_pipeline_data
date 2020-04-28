
# Data files
# three files = three experiments
# "Y202 All metrics 21_3_19.xlsx"  exp1
#    Y202 + Y201 CM corrected to Y202 + Y201CM
#    Y202 + Y202 CM corrected to Y202 + Y202CM
# "Y202 All metrics 25_3_19.xlsx"  exp2
# "Y202 All metrics 8_7_19.xlsx"   exp3

################################################################################
#                                   PACKAGES                                   #
################################################################################
library(tidyverse)
library(readxl)
library(janitor)
library(GGally)
library(caret)
library(lme4)




################################################################################
#                                   IMPORT                                     #
################################################################################
file1 <- "data_raw/Y202 All metrics 21_3_19.xlsx"
file2 <- "data_raw/Y202 All metrics 25_3_19.xlsx"
file3 <- "data_raw/Y202 All metrics 8_7_19.xlsx"

exp1 <- read_excel(file1) %>% clean_names()
exp2 <- read_excel(file2) %>% clean_names()
exp3 <- read_excel(file3) %>% clean_names()


################################################################################
#                                   COMBINE                                    #
################################################################################

# add experiment number to each dataframe
exp1$experiment <- "exp1"
exp2$experiment <- "exp2"
exp3$experiment <- "exp3"

alldata <- bind_rows(exp1, exp2, exp3)

# write.table(names(alldata), quote = TRUE,
#  row.names = FALSE, file = "varnames.txt")


################################################################################
#                             CREATE UTILITIES                                 #
################################################################################

measure_vars <- c("area_shape_area",
                 "area_shape_compactness",
                 "area_shape_eccentricity",
                 "area_shape_extent",
                 "area_shape_form_factor",
                 "area_shape_major_axis_length",
                 "area_shape_max_feret_diameter",
                 "area_shape_maximum_radius",
                 "area_shape_mean_radius",
                 "area_shape_median_radius",
                 "area_shape_min_feret_diameter",
                 "area_shape_minor_axis_length",
                 "area_shape_perimeter",
                 "area_shape_solidity",
                 "intensity_integrated_intensity_edge_split_redand_green",
                 "intensity_integrated_intensity_split_redand_green",
                 "intensity_lower_quartile_intensity_split_redand_green",
                 "intensity_mad_intensity_split_redand_green",
                 "intensity_mass_displacement_split_redand_green",
                 "intensity_max_intensity_edge_split_redand_green",
                 "intensity_max_intensity_split_redand_green",
                 "intensity_mean_intensity_edge_split_redand_green",
                 "intensity_mean_intensity_split_redand_green",
                 "intensity_median_intensity_split_redand_green",
                 "intensity_min_intensity_edge_split_redand_green",
                 "intensity_min_intensity_split_redand_green",
                 "intensity_std_intensity_edge_split_redand_green",
                 "intensity_std_intensity_split_redand_green",
                 "intensity_upper_quartile_intensity_split_redand_green")

group_var <- c("image_number", "experiment")

units = "in"
fig_w <- 6
fig_h <- 4
dpi <- 300
device <- "tiff" # this is format often required by journals; you may want png or jpg

################################################################################
#                                     EXPLORE                                  #
################################################################################

alldata %>% 
  group_by(image_number, experiment) %>% 
  summarise(n = length(image_number))

# image_number  experiment     n
# 1 Y202 + DMEM   exp1         128
# 2 Y202 + DMEM   exp2         139
# 3 Y202 + DMEM   exp3          74
# 4 Y202 + Y201CM exp1         127
# 5 Y202 + Y201CM exp2         117
# 6 Y202 + Y201CM exp3          82
# 7 Y202 + Y202CM exp1         120
# 8 Y202 + Y202CM exp2         128
# 9 Y202 + Y202CM exp3          94

alldata %>% 
  select(measure_vars) %>% 
  GGally::ggpairs() 

#  
# clust <- alldata %>% 
#   select(all_of(measure_vars)) %>% 
#   dist() %>% 
#   hclust(method = "complete")
# 
# plot(clust)

################################################################################
#                                     PCA                                      #
################################################################################
pca <- alldata %>%
  select(measure_vars) %>%
  prcomp(scale. = TRUE)

summary(pca)
# 
View(pca$rotation)

# put these in one 'tidy' dataframe
pca_labelled <- data.frame(pca$x, 
                           treatment = alldata$image_number, 
                           experiment = alldata$experiment)
#  scatterplot
pc1_pc2 <- ggplot(pca_labelled, 
       aes(x = PC1, y = PC2, color = treatment))+
  geom_point() +
  facet_grid(.~ experiment)

ggsave("pc1_pc2.tiff", 
       plot = pc1_pc2, 
       device = device,
       width = fig_w, 
       height = fig_w,
       units = units,
       dpi = dpi)



pc1_violin <- ggplot(pca_labelled, 
       aes(x = experiment , colour = treatment, y = PC1))+
  geom_violin()
ggsave("pc1_violin.tiff", 
       plot = pc1_violin, 
       device = device,
       width = fig_w, 
       height = fig_w,
       units = units,
       dpi = dpi)

pc1_boxplot <- ggplot(pca_labelled, 
       aes(x = experiment , colour = treatment, y = PC1))+
  geom_boxplot()
ggsave("pc1_boxplot.tiff", 
       plot = pc1_boxplot, 
       device = device,
       width = fig_w, 
       height = fig_w,
       units = units,
       dpi = dpi)

################################################################################
#                                     LDA                                      #
################################################################################

# create training and testing sets
ids <- createDataPartition(y = alldata$image_number, p = 0.75)[[1]]
train <- alldata[ids,] # those rows only
test <- alldata[-ids,] # without those rows

# train
ldaout <- train %>% 
  select(measure_vars) %>% 
  MASS::lda(grouping = train$image_number)

# And predict classes of the test data based on lda model
pldaout <- test %>% 
  select(measure_vars) %>%
  predict(object = ldaout)

# How many predicted classes are the same as the actual classes:
  
table(predicted = pldaout$class, observed = test$image_number)
#                 observed
# predicted       Y202 + DMEM Y202 + Y201CM Y202 + Y202CM
# Y202 + DMEM            68             2            17
# Y202 + Y201CM           2            66            10
# Y202 + Y202CM          15            13            58

  
# scatter plot of LD1 and LD2 :
  
# we add the labels of the original and class prediction
ldaout_labelled <- data.frame(pldaout$x,
                              observed_treatment = test$image_number,
                                predicted_treatment = pldaout$class)
ldaout_labelled$match <- ldaout_labelled$observed_treatment == ldaout_labelled$predicted_treatment

# a then to do a scatterplot
lda_test <- ggplot(ldaout_labelled, aes(x = LD1, y = LD2, 
                            colour = observed_treatment,
                            shape = match)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1)) +
  theme_classic()


ggsave("lda_test.tiff", 
       plot = lda_test, 
       device = device,
       width = fig_w, 
       height = fig_w,
       units = units,
       dpi = dpi)



################################################################################
#                           repeated measures                                  #
################################################################################

# using pca score

mod <- lmer(PC1 ~ treatment + (1|experiment), data = pca_labelled, REML=FALSE)
summary(mod)


modnull <- lmer(PC1 ~ 1 + (1|experiment), data = pca_labelled, REML=FALSE)

# Likelihood Ratio Test to compare the two models
anova(modnull, mod)
# Data: pca_labelled
# Models:
#   modnull: PC1 ~ 1 + (1 | experiment)
# mod: PC1 ~ treatment + (1 | experiment)
#         npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)    
# modnull    3 5201.5 5216.3 -2597.8   5195.5                        
# mod        5 4896.1 4920.7 -2443.1   4886.1 309.4  2  < 2.2e-16 ***
# Model with treatment is significantly better (data significantly more likely)
coef(mod)
