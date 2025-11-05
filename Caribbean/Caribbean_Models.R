# GOAL: Combine all of the data sources into one dataframe,
#  then create a GLMM with relevant variables

library(lme4)

##### Caribbean Eleutherodactylus #####
dist1 <- read.csv("Caribbean/isolation_Carib_Frogs.csv")
dist2 <- read.csv("Caribbean/no_occ_dists_Carib_Frogs.csv")

tri <- read.csv("Caribbean/Caribbean_Avg_TRI.csv")
elev <- read.csv("Caribbean/Caribbean_Elevation_Stats.csv")

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
frog_dat <- read.csv("Caribbean/Data/Eleutherodactylus_Curated.csv")
richness <- as.data.frame(table(frog_dat$First))
colnames(richness) <- c("name", "richness")
richness$name <- as.character(richness$name)
richness$richness <- as.numeric(richness$richness)
# Some names don't match
richness[1,1] <- "Andros"
richness[7,1] <- "Beef"
richness[8,1] <- "Bell Island"
richness[10,1] <- "Cat"
richness[11,1] <- "Cayman Brac Island"
richness[13,1] <- "de Culebra"
richness[16,1] <- "Eleuthera"
richness[17,1] <- "Grand Bahama"
richness[22,1] <- "Guana"
richness[25,1] <- "Juventud"
richness[26,1] <- "Mona"
richness[30,1] <- "Long"
richness[36,1] <- "North Caicos"
richness[40,1] <- "St. Croix"
richness[41,1] <- "St. John"
richness[43,1] <- "Saint Martin/Sint Maarten"
richness[45,1] <- "Shroud Cay"
richness[46,1] <- "South Bimini Cay"
richness[51,1] <- "Tortue"
richness[52,1] <- "Viegues"
richness[54,1] <- "West End Cay"

# Merge richness to final dataframe
final_df <- merge(final_df, richness, by = "name", all.x = TRUE, all.y = TRUE)

# Add areas to final dataframe
areas_df <- read.csv("Caribbean/Data/carib_areas.csv")
final_df <- merge(final_df, areas_df, by = "name", all.x = TRUE)

# Add CSI to final dataframe
csi <- read.csv("Caribbean/Data/csi.csv")
colnames(csi) <- c("name", "mean_csi", "sd_csi")
final_df <- merge(final_df, csi, by = "name", all.x = TRUE)

# Replace NAs with 0
final_df[is.na(final_df)] <- 0

# Remove rows where area = 0 (that means no island info)
final_df <- final_df[-which(final_df$area == 0),]

# Add whether the island is before/after the breakpoint
# The breakpoint here is 21.114, so any log(area) above that is after the BP
bp <- rep(0, 408)
for(i in c(1:length(final_df$area))){
  if(log(final_df[i,10]) > 21.114){
    bp[i] <- 1
  }
}

final_df <- cbind(final_df, bp)

write.csv(final_df, "Caribbean/carib_eleutherodactylus_final_df.csv", row.names = FALSE)

# Run GLMM?
# The Eleutherodactylus SAR has a breakpoint and a positive second slope
# nearest_occ might actually be fine to include here
model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + (1 | bp), 
                data = final_scale, family = poisson)
model2 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + area + (1 | bp), 
                data = final_df, family = poisson)

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
for(i in c(1:length(final_df))){
  if(log(final_df[i,10]) > 20.709){
    bp[i] <- 1
  }
}

final_df <- cbind(final_df, bp)

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
