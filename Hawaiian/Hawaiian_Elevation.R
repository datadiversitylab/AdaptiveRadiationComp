library(elevatr)
library(sf)
library(raster)
library(spatialEco)

# Read in shapefile from the State of Hawaii Office of Planning and Sustainable Development
# (https://planning.hawaii.gov/gis/download-gis-data-expanded/)
hawaii <- st_read("Hawaiian/Shapefile/coastline.shp")

# Before using this shapefile:
# Remove all polygons with NA name
hawaii_names <- hawaii[!is.na(hawaii$isle), ]

# Ensure the polygons are gone
hawaii_final <- hawaii_names[!is.empty(hawaii_names), ]

# Ensure that all names are unique
hawaii_final$isle <- make.unique(hawaii_final$isle)

# Need to transform to WGS84
hawaii_wgs84 <- st_transform(hawaii_final, crs = 4326)

# Access AWS Terrain Tiles and Open Topography to access DEM
elevation <- get_elev_raster(locations = hawaii_wgs84, prj = "+proj=longlat +datum=WGS84", z = 10)

# Use spatialEco package to get relevant elevation statistics

## Terrain Ruggedness Index (TRI)
# First, it need to be a SpatRaster
elev_rast <- as(elevation, "SpatRaster")

# Run tri function
tri_hawaii <- tri(elev_rast)

# Crop and mask the TRI to the area of interest
elev_crop <- crop(tri_hawaii, hawaii_wgs84)
elev_mask <- mask(elev_crop, hawaii_wgs84)
plot(elev_mask)

# Mean TRI per island (need terra extract specifically because it is a SpatRaster)
island_tri <- terra::extract(elev_mask, hawaii_wgs84, fun = mean)

# Add island names
island_tri$name <- hawaii_wgs84$isle

# Remove extraneous ID column
island_tri <- island_tri[,-1]
# Name TRI column
colnames(island_tri)[1] <- "TRI"

# Write CSV
write.csv(island_tri, "Hawaiian/Hawaii_Avg_TRI.csv", row.names = FALSE)

## Summary stats (mean, median, min, max elevation)
# Use the elev_rast object (base DEM)
elev_mean <- terra::extract(elev_rast, hawaii_wgs84, fun = mean)
colnames(elev_mean)[2] <- "mean_elev"
elev_median <- terra::extract(elev_rast, hawaii_wgs84, fun = median)
colnames(elev_median)[2] <- "median_elev"
elev_min <- terra::extract(elev_rast, hawaii_wgs84, fun = min)
colnames(elev_min)[2] <- "min_elev"
elev_max <- terra::extract(elev_rast, hawaii_wgs84, fun = max)
colnames(elev_max)[2] <- "max_elev"

# Combine all together
elev_sum <- cbind(island_tri[2], elev_mean[2], elev_median[2], elev_min[2], elev_max[2])

# Write CSV
write.csv(elev_sum, "Hawaiian/Hawaiian_Elevation_Stats.csv", row.names = FALSE)
