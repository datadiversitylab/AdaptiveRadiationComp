# GOAL: Find out distance to closest mainland for each island

# Step 1: Use the global polygons from Natural Earth and choose 4 largest
# (Antarctica, North/South America, Africa/Eurasia, Australia)

library(rnaturalearth)
library(sf)

# Returns as sf object by default. Would SpatVector be better?
land <- ne_download(
  scale = "medium",
  type = "land",
  category = "physical"
)

# Change MULTIPOLYGON to regular POLYGON in the sf object
poly_land <- st_cast(land, "POLYGON")

# Calculate area for each polygon
poly_land$area <- st_area(poly_land)

# Slice the 4 largest polygons
continents <- poly_land %>%
  arrange(desc(area)) %>%
  slice(1:4)

plot(continents)

# North/South America area: 37700259639829.5
# Africa/Eurasia area: 79366983484055
# Antarctica area: 12064404832080
# Australia area: 7616686731658
# Add the names to the continent object
name <- c("Africa_Eurasia", "North_South_America", "Antarctica", "Australia")
continents$name <- name

# Save as an RDS for now
saveRDS(continents, file = "continents.rds")

## Step 2: Find the shortest distance to any continent polygon from each island
continents <- readRDS("continents.rds")

# Galapagos #
# I can't use the centroid method from the island-to-island distance calculation
#  because the centroid of these huge landmasses would inflate the distance

# Find polygon-to-polygon distance?
# Using galap_final from Galapagos_Elevation.R
# Create a distance matrix
dist_mat <- st_distance(galap_final, continents)

# Find the minimum distance for each island
min_dist <- rep(0, nrow(dist_mat))
for(i in c(1:nrow(dist_mat))){
  min_dist[i] <- min(dist_mat[i,])
}

# Add each island's name to the dataframe
# (row numbers are in order of the names in the sf object)
distance_df <- data.frame(
  island_name = galap_final$nombre,
  dist_mainland_m = min_dist
)

write.csv(distance_df, "Galapagos/Data/distance_to_mainland_GalapIslands.csv")

# Hawaii #
# Using hawaii_wgs84 from Hawaiian_Elevation.R
# Create a distance matrix
dist_mat <- st_distance(hawaii_wgs84, continents)

# Find the minimum distance for each island
min_dist <- rep(0, nrow(dist_mat))
for(i in c(1:nrow(dist_mat))){
  min_dist[i] <- min(dist_mat[i,])
}

# Add each island's name to the dataframe
# (row numbers are in order of the names in the sf object)
distance_df <- data.frame(
  island_name = hawaii_wgs84$isle,
  dist_mainland_m = min_dist
)

write.csv(distance_df, "Hawaiian/Data/distance_to_mainland_HawaiianIslands.csv")

# Caribbean #
# Using carib_final from Caribbean_Elevation.R
# Create a distance matrix
dist_mat <- st_distance(carib_final, continents)

# Find the minimum distance for each island
min_dist <- rep(0, nrow(dist_mat))
for(i in c(1:nrow(dist_mat))){
  min_dist[i] <- min(dist_mat[i,])
}

# Add each island's name to the dataframe
# (row numbers are in order of the names in the sf object)
distance_df <- data.frame(
  island_name = carib_final$Name_USGSO,
  dist_mainland_m = min_dist
)

write.csv(distance_df, "Caribbean/Data/distance_to_mainland_CaribIslands.csv")
