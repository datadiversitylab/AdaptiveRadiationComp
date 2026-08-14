# Estimating habitat heterogeneity for each lineage of interest
# NOTE: Assumes that the working directory is the root of the GitHub repo

# Load libraries
library(terra)
library(sf)
library(dplyr)

# This file is created in 1_crop_habitat_raster.R
# It is unfortunately too big to include in the GitHub repo
habitat_lvl2 <- rast("iucn_habitatclassification_composite_lvl2_ver004_sub.tif")

##### Hawaii #####

# HawaiianAsteraceae (from IUCN; geo [hawaii st.] and taxonomic [asteraceae] restriction)
ranges <- vect("habitat_diversity/HawaiianAsteraceae/data_0.shp")

# ISLAND-SPECIFIC
# Using hawaii_wgs84 object from "Hawaii/Hawaiian_Elevation.R"
hawaii_wgs84 <- readRDS("Hawaiian/Data/hawaii_wgs84.rds")
# Crop ranges
range_crop <- crop(ranges, hawaii_wgs84)
# Change hawaii_wgs84 to SpatVector
hawaii_vec <- vect(hawaii_wgs84)
combo_range <- intersect(range_crop, hawaii_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$isle)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$isle == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Hawaiian/Data/n_habitat_silverswords.csv", row.names = FALSE)

# Tetragnatha (buffer around gbif occurrences)

# library(rgbif)
# tetragnatha_key <- name_backbone("Tetragnatha")$usageKey
# tetragnatha_data <- occ_search(
#   taxonKey = tetragnatha_key,
#   country = "US",
#   stateProvince = "Hawaii",
#   hasCoordinate = TRUE
# )
# occur_data <- tetragnatha_data$data
# occur_clean <- occur_data %>%
#   filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
#   filter(coordinateUncertaintyInMeters < 100 | is.na(coordinateUncertaintyInMeters)) # Optional: filter by uncertainty
# 
# # To cite GBIF properly, make sure to get info for a Derived Dataset
# library(ssarp)
# sources <- get_sources(occur_clean)
# 
# occur_points <- vect(occur_clean, 
#                      geom = c("decimalLongitude", "decimalLatitude"),
#                      crs = "EPSG:4326")
dat <- read.csv("Hawaiian/Data/tetragnatha_filtered_occs.csv")

occur_points <- vect(dat,
                     geom = c("decimalLongitude", "decimalLatitude"),
                     crs = "EPSG:4326")

occur_buffers <- buffer(occur_points, width = 10) #10 meters buffer

# Do the same thing as for Silverswords to get number of habitats
combo_range <- terra::intersect(occur_buffers, hawaii_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$isle)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$isle == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Hawaiian/Data/n_habitat_tetragnatha.csv", row.names = FALSE)

##### Galapagos #####

# Galapagos Finches (from IUCN; geo [galapagos] and taxonomic [thraupidae] restriction)
ranges <- vect(here("habitat_diversity", "GalapagosFinches/data_0.shp"))

# Using galap_final object from "Galapagos/Galapagos_Elevation.R"
galap_final <- readRDS("Galapagos/Data/galap_final.rds")
# Crop ranges
range_crop <- crop(ranges, galap_final)
# Change galap_final to SpatVector
galap_vec <- vect(galap_final)
combo_range <- terra::intersect(range_crop, galap_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$nombre)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$nombre == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Galapagos/Data/n_habitat_finches.csv", row.names = FALSE)

## Scalesia with GBIF ##
# Using galap_final object from "Galapagos/Galapagos_Elevation.R"
galap_final <- readRDS("Galapagos/Data/galap_final.rds")
# Change galap_final to SpatVector
galap_vec <- vect(galap_final)

dat <- read.csv("Galapagos/Data/scalesia_occs_NEW.csv")

occur_points <- vect(dat,
                     geom = c("decimalLongitude", "decimalLatitude"),
                     crs = "EPSG:4326")

occur_buffers <- buffer(occur_points, width = 10) #10 meters buffer

# Do the same thing as for Silverswords to get number of habitats
combo_range <- terra::intersect(occur_buffers, galap_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$nombre)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$nombre == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Galapagos/Data/n_habitat_scalesia.csv", row.names = FALSE)

##### Caribbean #####

# Caribbean Anoles (from IUCN; geo [caribbean] and taxonomic [anolis] restriction)
# Clipped to the caribbean using QGIS (data_0.shp -> systems.shp; memory issues in R)
# Available at https://nextcloud.datadiversitylab.synology.me/s/sWzMnAWK43w9LDd
#ranges <- vect(here("habitat_diversity", "CaribbeanAnoles/data_1.shp"))
ranges <- vect("data_1.shp")

# Using carib_final object from "Caribbean/Caribbean_Elevation.R"
carib_final <- readRDS("Caribbean/Data/carib_final.rds")
# Crop ranges
range_crop <- crop(ranges, carib_final)
# Change carib_final to SpatVector
carib_vec <- vect(carib_final)
combo_range <- intersect(range_crop, carib_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$Name_USGSO)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$Name_USGSO == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Caribbean/Data/n_habitat_anolis.csv", row.names = FALSE)

# Caribbean Eleuterodactylus (from IUCN; geo [caribbean] and taxonomic [eleuterodactylus] restriction)
ranges <- vect(here("habitat_diversity", "CaribbeanEleuterodactylus/data_0.shp"))

# Using carib_final object from "Caribbean/Caribbean_Elevation.R"
carib_final <- readRDS("Caribbean/Data/carib_final.rds")
# Crop ranges
range_crop <- crop(ranges, carib_final)

combo_range <- intersect(range_crop, carib_vec)

# Loop through each island/range combo polygon and grab the
#  number of habitats using extract

# Get island names
islands <- unique(combo_range$Name_USGSO)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island/range combo polygon for single island
  single_island <- combo_range[combo_range$Name_USGSO == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Caribbean/Data/n_habitat_frogs.csv", row.names = FALSE)
