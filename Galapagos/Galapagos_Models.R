# GOAL: Combine all of the data sources into one dataframe,
#  then create a GLMM with relevant variables

library(lme4)

##### Galapagos Finches #####
dist1 <- read.csv("Galapagos/isolation_Galap_Finch.csv")
dist2 <- read.csv("Galapagos/no_occ_dists_Galap_Finch.csv")

tri <- read.csv("Galapagos/Galap_Avg_TRI.csv")
elev <- read.csv("Galapagos/Galap_Elevation_Stats.csv")

# Ensure that all dataframes include the same islands (dictated by dist1)
tri <- tri[tri$name %in% dist1$Island, ]
elev <- elev[elev$name %in% dist1$Island, ]

# Merge dataframes together
dem_stats <- merge(tri, elev, by = "name", all.x = TRUE, all.y = TRUE)
isolation <- merge(dist1, dist2, by = "Island", all.x = TRUE, all.y = TRUE)
colnames(isolation)[3] <- "nearest_occ"
colnames(isolation)[1] <- "name"

# Now merge to create the final dataframe
final_df <- merge(dem_stats, isolation, by = "name", all.x = TRUE, all.y = TRUE)

# Get richness column
geo_dat <- read.csv("Galapagos/Data/finch_pam_areas_2.csv")
richness <- as.data.frame(table(geo_dat$Third))
colnames(richness) <- c("name", "richness")
# Some of the island names don't match the rest of the data
richness[,1] <- as.character(richness[,1])
richness[9,1] <- "Seymour Norte"
richness[15,1] <- "Santa Fé"
richness[3,1] <- "Española"
richness[11,1] <- "Pinzón"

# Merge richness to final dataframe
final_df <- merge(final_df, richness, by = "name", all.x = TRUE, all.y = TRUE)

# Add areas to final dataframe
areas_df <- read.csv("Galapagos/Data/galapagos_areas.csv")
final_df <- merge(final_df, areas_df, by = "name", all.x = TRUE)

# Add CSI to final dataframe
csi <- read.csv("Galapagos/Data/csi.csv")
colnames(csi) <- c("name", "mean_csi", "sd_csi")
final_df <- merge(final_df, csi, by = "name", all.x = TRUE)

# Replace NAs with 0
final_df[is.na(final_df)] <- 0

write.csv(final_df, "galap_finch_final_df.csv", row.names = FALSE)

# Run GLMM?
# model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + nearest_occ, 
#                data = final_df, family = poisson)

# The finch SAR doesn't have a breakpoint, so actually it should be a glm?
model1 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + nearest_occ + mean_csi + sd_csi,
              family = poisson, data = final_df)
model2 <- glm(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + nearest_occ + mean_csi + sd_csi + area,
              family = poisson, data = final_df)

##### Galapagos Scalesia #####

dist1 <- read.csv("Galapagos/isolation_Galap_Scalesia.csv")
dist2 <- read.csv("Galapagos/no_occ_dists_Galap_Scalesia.csv")

tri <- read.csv("Galapagos/Galap_Avg_TRI.csv")
elev <- read.csv("Galapagos/Galap_Elevation_Stats.csv")

# Ensure that all dataframes include the same islands (dictated by dist1)
tri <- tri[tri$name %in% dist1$Island, ]
elev <- elev[elev$name %in% dist1$Island, ]

# Merge dataframes together
dem_stats <- merge(tri, elev, by = "name", all.x = TRUE, all.y = TRUE)
isolation <- merge(dist1, dist2, by = "Island", all.x = TRUE, all.y = TRUE)
colnames(isolation)[3] <- "nearest_occ"
colnames(isolation)[1] <- "name"

# Now merge to create the final dataframe
final_df <- merge(dem_stats, isolation, by = "name", all.x = TRUE, all.y = TRUE)

# Get richness column
scal_dat <- read.csv("Galapagos/Data/Scalesia_Areas.csv")
richness <- as.data.frame(table(scal_dat$Second))
colnames(richness) <- c("name", "richness")
# Some of the island names don't match the rest of the data
richness[,1] <- as.character(richness[,1])
richness[6,1] <- "Pinzón"
richness[9,1] <- "Santa Fé"

# Merge richness to final dataframe
final_df <- merge(final_df, richness, by = "name", all.x = TRUE, all.y = TRUE)

# Add areas to final dataframe
areas_df <- read.csv("Galapagos/Data/galapagos_areas.csv")
final_df <- merge(final_df, areas_df, by = "name", all.x = TRUE)

# Add CSI to final dataframe
csi <- read.csv("Galapagos/Data/csi.csv")
colnames(csi) <- c("name", "mean_csi", "sd_csi")
final_df <- merge(final_df, csi, by = "name", all.x = TRUE)

# Replace NAs with 0
final_df[is.na(final_df)] <- 0

# Add whether the island is before/after the breakpoint
# The breakpoint here is 20.709, so any log(area) above that is after the BP
bp <- rep(0, 12)
for(i in c(1:length(final_df$name))){
  if(log(final_df[i,10]) > 20.709){
    bp[i] <- 1
  }
}

final_df <- cbind(final_df, bp)

write.csv(final_df, "galap_scalesia_final_df.csv", row.names = FALSE)

# Try to scale the columns?
final_scale <- scale(final_df[,2:12])
final_scale <- cbind(final_df$name, final_scale, final_df$bp)
final_scale <- as.data.frame(final_scale)
colnames(final_scale)[1] <- "name"
colnames(final_scale)[13] <- "bp"
# They're all characters for some reason
final_scale[,2:13] <- sapply(final_scale[,2:13], as.numeric)

# I don't think we want to scale richness though...
final_scale$richness <- final_df$richness

# Run GLMM?
# The Scalesia SAR has a breakpoint and a negative second slope
# nearest_occ has too many zeros for the model to run without error
model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + (1 | bp), 
                data = final_scale, family = poisson)
model2 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area + (1 | bp), 
                data = final_df, family = poisson)
