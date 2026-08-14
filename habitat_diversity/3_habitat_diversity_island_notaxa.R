# GOAL: Find the number of habitats (as designated by the IUCN) for each island
#       in each archipelago of interest

library(terra)
library(sf)
library(dplyr)
library(raster)

# Load habitat raster
habitat_lvl2 <- rast("iucn_habitatclassification_composite_lvl2_ver004_sub.tif")

##### Hawaii #####

# Using hawaii_wgs84 object from "Hawaii/Hawaiian_Elevation.R"
hawaii_wgs84 <- readRDS("Hawaiian/Data/hawaii_wgs84.rds")

# Change hawaii_wgs84 to SpatVector
hawaii_vec <- vect(hawaii_wgs84)

# Get island names
islands <- unique(hawaii_vec$isle)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island polygon for single island
  single_island <- hawaii_vec[hawaii_vec$isle == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Hawaiian/Data/n_habitat_hawaiian_islands.csv", row.names = FALSE)

##### Galapagos #####
# Using galap_final object from "Galapagos/Galapagos_Elevation.R"
galap_final <- readRDS("Galapagos/Data/galap_final.rds")

# Change galap_final to SpatVector
galap_vec <- vect(galap_final)

# Get island names
islands <- unique(galap_vec$nombre)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island polygon for single island
  single_island <- galap_vec[galap_vec$nombre == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Galapagos/Data/n_habitat_galapagos_islands.csv", row.names = FALSE)

##### Caribbean #####
# Using carib_final object from "Caribbean/Caribbean_Elevation.R"
carib_final <- readRDS("Caribbean/Data/carib_final.rds")

# Change carib_final to SpatVector
carib_vec <- vect(carib_final)

# Get island names
islands <- unique(carib_vec$Name_USGSO)
# Create empty list for results
results <- list()

for(i in islands){
  # Grab island polygon for single island
  single_island <- carib_vec[carib_vec$Name_USGSO == i, ]
  
  # Add extracted values to the results
  results[[i]] <- extract(habitat_lvl2, single_island)
}

# Now, for each island, get the number of habitats
habitat_df <- as.data.frame(matrix(nrow = length(islands), ncol = 2))
for(i in c(1:length(islands))){
  habitat_df[i,] <- c(islands[i], length(unique(results[[i]][,2])))
}

colnames(habitat_df) <- c("Name", "n_habitat")
write.csv(habitat_df, "Caribbean/Data/n_habitat_caribbean_islands.csv", row.names = FALSE)
