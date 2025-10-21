library(elevatr)
library(sf)
library(raster)
library(spatialEco)

# Read in shapefile cropped from the new USGS global island dataset
# (https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273)
carib <- st_read("Caribbean/Shapefile/caribbean_test2.shp")

# Before using this shapefile:
# Remove all polygons with NA name
carib_names <- carib[!is.na(carib$Name_USGSO), ]

# Also remove all polygons with "UNNAMED" as a name
all_names <- carib_names$Name_USGSO
keep_names <- all_names[!all_names %in% "UNNAMED"]
# Filter to keep_names
carib_final <- carib_names[carib_names$Name_USGSO %in% keep_names, ]

# Ensure the polygons are gone
# carib_final <- carib_names[!is.empty(carib_names), ]
# ^ This is taking too long, but I think the script works fine without it

# Ensure that all names are unique
carib_final$Name_USGSO <- make.unique(carib_final$Name_USGSO)

# Access AWS Terrain Tiles and Open Topography to access DEM
elevation <- get_elev_raster(locations = carib_final, prj = "+proj=longlat +datum=WGS84", z = 10)
# WARNING: almost 1 GB
# writeRaster(elevation, filename = "Caribbean/Data/elevation.tif", format = "GTiff")
elevation <- raster("Caribbean/Data/elevation.tif")

# Use spatialEco package to get relevant elevation statistics

## Terrain Ruggedness Index (TRI)
# First, it need to be a SpatRaster
elev_rast <- as(elevation, "SpatRaster")

# Run tri function
tri_carib <- tri(elev_rast)

# Crop and mask the TRI to the area of interest
elev_crop <- crop(tri_carib, carib_final)
elev_mask <- mask(elev_crop, carib_final)
plot(elev_mask)

# Mean TRI per island (need terra extract specifically because it is a SpatRaster)
island_tri <- terra::extract(elev_mask, carib_final, fun = mean)

# Add island names
island_tri$name <- carib_final$Name_USGSO

# Remove extraneous ID column
island_tri <- island_tri[,-1]
# Name TRI column
colnames(island_tri)[1] <- "TRI"

# Write CSV
write.csv(island_tri, "Caribbean/Caribbean_Avg_TRI.csv", row.names = FALSE)

## Summary stats (mean, median, min, max elevation)
# Use the elev_rast object (base DEM)
elev_mean <- terra::extract(elev_rast, carib_final, fun = mean)
colnames(elev_mean)[2] <- "mean_elev"
elev_median <- terra::extract(elev_rast, carib_final, fun = median)
colnames(elev_median)[2] <- "median_elev"
elev_min <- terra::extract(elev_rast, carib_final, fun = min)
colnames(elev_min)[2] <- "min_elev"
elev_max <- terra::extract(elev_rast, carib_final, fun = max)
colnames(elev_max)[2] <- "max_elev"

# Combine all together
elev_sum <- cbind(island_tri[2], elev_mean[2], elev_median[2], elev_min[2], elev_max[2])

# Write CSV
write.csv(elev_sum, "Caribbean/Caribbean_Elevation_Stats.csv", row.names = FALSE)
