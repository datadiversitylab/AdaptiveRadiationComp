# GOAL: Combine all of the data sources into one dataframe,
#  then create a GLMM with relevant variables

library(lme4)

##### Hawaiian Tetragnatha #####
dist1 <- read.csv("Hawaiian/isolation_Hawaii_Tetragnatha.csv")
# No dist2 because all islands within the size range have occurrences

tri <- read.csv("Hawaiian/Hawaii_Avg_TRI.csv")
elev <- read.csv("Hawaiian/Hawaiian_Elevation_Stats.csv")

# Ensure that all dataframes include the same islands (dictated by dist1)
tri <- tri[tri$name %in% dist1$Island, ]
elev <- elev[elev$name %in% dist1$Island, ]

# Merge dataframes together
dem_stats <- merge(tri, elev, by = "name", all.x = TRUE, all.y = TRUE)
#isolation <- merge(dist1, dist2, by = "Island", all.x = TRUE, all.y = TRUE)
isolation <- dist1
colnames(isolation)[1] <- "name"

# Now merge to create the final dataframe
final_df <- merge(dem_stats, isolation, by = "name", all.x = TRUE, all.y = TRUE)

# Get richness column
tet_dat <- read.csv("Hawaiian/Data/Tetragnatha_Areas.csv")
richness <- as.data.frame(table(tet_dat$Second))
colnames(richness) <- c("name", "richness")
# Names need to be characters
richness[,1] <- as.character(richness[,1])

# Merge richness to final dataframe
final_df <- merge(final_df, richness, by = "name", all.x = TRUE, all.y = TRUE)

# Add areas to final dataframe
areas_df <- read.csv("Hawaiian/Data/hawaii_areas.csv")
final_df <- merge(final_df, areas_df, by = "name", all.x = TRUE)

# Add CSI to final dataframe
csi <- read.csv("Hawaiian/Data/csi.csv")
colnames(csi) <- c("name", "mean_csi", "sd_csi")
final_df <- merge(final_df, csi, by = "name", all.x = TRUE)

# Replace NAs with 0
final_df[is.na(final_df)] <- 0

# Try to scale the columns?
final_scale <- scale(final_df[,2:11])
final_scale <- cbind(final_df$name, final_scale)
final_scale <- as.data.frame(final_scale)
colnames(final_scale)[1] <- "name"
# They're all characters for some reason
final_scale[,2:11] <- sapply(final_scale[,2:11], as.numeric)

# I don't think we want to scale richness though...
final_scale$richness <- final_df$richness

## Run models

# The Tetragnatha SAR doesn't have a breakpoint, so actually it should be a glm?
model1 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi,
              family = poisson, data = final_scale)
model2 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area,
              family = poisson, data = final_scale)

# Check out the correlation of the numeric predictors (but not richness)
cor_matrix <- cor(final_df[, c("TRI", "mean_elev", "median_elev", "min_elev", 
                               "max_elev", "Nearest_Dist", 
                               "mean_csi", "sd_csi", "area")])
# TRI: median_elev, area
# mean_elev: median_elev, min elev, max elev, mean_csi, sd_csi, area
# median_elev: TRI, mean_elev, min_elev, max_elev, mean_csi, sd_csi, area
# min_elev: mean_elev, median_elev, max_elev
# max_elev: mean_elev, median_elev, min_elev, mean_csi, sd_csi, area
# Nearest_Dist: mean_csi
# mean_csi: mean_elev, median_elev, max_elev, Nearest_Dist, sd_csi, area
# sd_csi: mean_elev, median_elev, max_elev, Nearest_Dist, mean_csi, area
# area: TRI, mean_elev, median_elev, max_elev, mean_csi, sd_csi

# REMOVE: mean_elev, median_elev, max_elev, sd_csi?
model1 <- glm(richness ~ TRI + min_elev + Nearest_Dist + mean_csi,
              family = poisson, data = final_scale)
model2 <- glm(richness ~ TRI + min_elev + Nearest_Dist + mean_csi + area,
              family = poisson, data = final_scale)

##### Hawaiian Silverswords #####

dist1 <- read.csv("Hawaiian/isolation_Hawaii_Silverswords.csv")
# No dist2 because all islands within the size range have occurrences

tri <- read.csv("Hawaiian/Hawaii_Avg_TRI.csv")
elev <- read.csv("Hawaiian/Hawaiian_Elevation_Stats.csv")

# Ensure that all dataframes include the same islands (dictated by dist1)
tri <- tri[tri$name %in% dist1$Island, ]
elev <- elev[elev$name %in% dist1$Island, ]

# Merge dataframes together
dem_stats <- merge(tri, elev, by = "name", all.x = TRUE, all.y = TRUE)
#isolation <- merge(dist1, dist2, by = "Island", all.x = TRUE, all.y = TRUE)
isolation <- dist1
colnames(isolation)[1] <- "name"

# Now merge to create the final dataframe
final_df <- merge(dem_stats, isolation, by = "name", all.x = TRUE, all.y = TRUE)

# Get richness column
silver_dat <- read.csv("Hawaiian/Data/silversword_richness.csv")
richness <- as.data.frame(table(silver_dat$Third))
colnames(richness) <- c("name", "richness")

# Merge richness to final dataframe
final_df <- merge(final_df, richness, by = "name", all.x = TRUE, all.y = TRUE)

# Add areas to final dataframe
areas_df <- read.csv("Hawaiian/Data/hawaii_areas.csv")
final_df <- merge(final_df, areas_df, by = "name", all.x = TRUE)

# Add CSI to final dataframe
csi <- read.csv("Hawaiian/Data/csi.csv")
colnames(csi) <- c("name", "mean_csi", "sd_csi")
final_df <- merge(final_df, csi, by = "name", all.x = TRUE)

# Add whether the island is before/after the breakpoint
# The breakpoint here is 21.496, so any log(area) above that is after the BP
bp <- rep(0, 6)
for(i in c(1:6)){
  if(log(final_df[i,9]) > 21.496){
    bp[i] <- 1
  }
}

final_df <- cbind(final_df, bp)

# Try to scale the columns?
final_scale <- scale(final_df[,2:11])
final_scale <- cbind(final_df$name, final_scale, final_df$bp)
final_scale <- as.data.frame(final_scale)
colnames(final_scale)[1] <- "name"
colnames(final_scale)[12] <- "bp"
# They're all characters for some reason
final_scale[,2:12] <- sapply(final_scale[,2:12], as.numeric)

# I don't think we want to scale richness though...
final_scale$richness <- final_df$richness

# Try creating a pca with the data, then use the PC axes as variables instead
final_pca <- prcomp(final_df[, c("min_elev", "max_elev", "median_elev", "mean_elev", "TRI", "Nearest_Dist", "mean_csi", "sd_csi")],
                    scale. = TRUE)

final_df$PC1 <- final_pca$x[,1]
final_df$PC2 <- final_pca$x[,2]
final_df$PC3 <- final_pca$x[,3]

# Run GLMM?
# The Silversword SAR has a breakpoint and a negative second slope
# DOES NOT CONVERGE
model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + (1 | bp), 
                data = final_scale, family = poisson)
model2 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area + (1 | bp), 
                data = final_df, family = poisson)

model3 <- glmer(richness ~ PC1 + PC2 + PC3 + (1|bp),
                data = final_df, family = poisson)
# Find out variable importance in model3
# For the important PC, look at weights (final_pca$rotation)

##### Attempting Variable Importance #####
library(lme4)
library(glmm.hp)
library(MuMIn)

## Environment object definitions:
# final_df - a dataframe of all of the unscaled predictors 
#            (including PC1, PC2, and PC3)
# final_pca - the PCA result, which incorporates all of the variables except for
#             richness, area, and bp
# model3 - the glmer model with formula richness ~ PC1 + PC2 + PC3 + (1|bp)

glmm.hp(model3)
# ERROR: object's call contains dotted names

# Maybe the function needs to be updatable first?
uglmer <- updateable(glmer)
model4 <- uglmer(richness ~ PC1 + PC2 + PC3 + (1|bp), data = final_df, family = poisson)
glmm.hp(model4)
# ERROR: object's call contains dotted names

# Maybe we can rank importance based on coefficients?
fixef(model3)
coefs <- fixef(model3)[c("PC1", "PC2", "PC3")]
abs(coefs) / sum(abs(coefs))

# Variable importance in the original space?
loadings <- final_pca$rotation
# Fancy matrix multiplication suggestion from Google
var_importance <- as.matrix(loadings[, 1:3]) %*% coefs

# Or maybe partial R^2 would be better?
library(performance)
r2_nakagawa(model3)
# This doesn't work for the same reason that glmm.hp doesn't work
