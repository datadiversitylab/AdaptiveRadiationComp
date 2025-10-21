library(elevatr)
library(sf)
library(raster)
library(spatialEco)

# Read in shapefile from the Galapagos Geoportal (https://geodata.fcdarwin.org.ec/catalogue/#/dataset/489)
galap <- st_read("Galapagos/Shapefile/galapagos_island_Project.shp")

# Before using this shapefile:
# Remove all polygons with NA name
galap_names <- galap[!is.na(galap$nombre), ]

# Ensure the polygons are gone
galap_final <- galap_names[!is.empty(galap_names), ]

# Ensure that all names are unique
galap_final$nombre <- make.unique(galap_final$nombre)

# Access AWS Terrain Tiles and Open Topography to access DEM
elevation <- get_elev_raster(locations = galap_final, prj = "+proj=longlat +datum=WGS84", z = 10)

# Use spatialEco package to get relevant elevation statistics

## Terrain Ruggedness Index (TRI)
# First, it need to be a SpatRaster
elev_rast <- as(elevation, "SpatRaster")

# Run tri function
tri_galap <- tri(elev_rast)

# Crop and mask the TRI to the area of interest
elev_crop <- crop(tri_galap, galap_final)
elev_mask <- mask(elev_crop, galap_final)
plot(elev_mask)

# Mean TRI per island (need terra extract specifically because it is a SpatRaster)
island_tri <- terra::extract(elev_mask, galap_final, fun = mean)

# Add island names
island_tri$name <- galap_final$nombre

# Remove extraneous ID column
island_tri <- island_tri[,-1]
# Name TRI column
colnames(island_tri)[1] <- "TRI"

# Write CSV
write.csv(island_tri, "Galap_Avg_TRI.csv", row.names = FALSE)

## Summary stats (mean, median, min, max elevation)
# Use the elev_rast object (base DEM)
elev_mean <- terra::extract(elev_rast, galap_final, fun = mean)
colnames(elev_mean)[2] <- "mean_elev"
elev_median <- terra::extract(elev_rast, galap_final, fun = median)
colnames(elev_median)[2] <- "median_elev"
elev_min <- terra::extract(elev_rast, galap_final, fun = min)
colnames(elev_min)[2] <- "min_elev"
elev_max <- terra::extract(elev_rast, galap_final, fun = max)
colnames(elev_max)[2] <- "max_elev"

# Combine all together
elev_sum <- cbind(island_tri[2], elev_mean[2], elev_median[2], elev_min[2], elev_max[2])

# Write CSV
write.csv(elev_sum, "Galapagos/Galap_Elevation_Stats.csv", row.names = FALSE)
