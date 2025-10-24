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

## Run models

# The Tetragnatha SAR doesn't have a breakpoint, so actually it should be a glm?
model1 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi,
              family = poisson, data = final_df)
model2 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area,
              family = poisson, data = final_df)

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

# Run GLMM?
# The Silversword SAR has a breakpoint and a negative second slope
# DOES NOT CONVERGE
model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + (1 | bp), 
                data = final_scale, family = poisson)
model2 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area + (1 | bp), 
                data = final_df, family = poisson)
