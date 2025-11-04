library(terra)
library(sf)
library(dplyr)
library(here)

# This script is used to crop the habitat classification
# raster to the target island systems

#https://zenodo.org/records/4058819


## Level 1

habitat_lvl1 <- rast(here("habitat_diversity", 
                          "iucn_habitatclassification_composite_lvl1_ver004.tif"))


##Read in shapefiles (islands)
caribbean <- vect(here("Caribbean", 
                       "Shapefile",
                       "caribbean_test2.shp"
))

hawaii <- vect(here("Hawaiian", 
                    "Shapefile",
                    "coastline.shp"
))
hawaii <- project(hawaii, "EPSG:4326")


galapagos <- vect(here("Galapagos", 
                       "Shapefile",
                       "galapagos_island_Project.shp"
))

# Objects delimiting the target areas
systems <- rbind(caribbean, galapagos, hawaii)

writeVector(systems, 
            here("habitat_diversity",
                 "systems.shp"),
            overwrite = FALSE)

habitat_lvl1.1 <- crop(habitat_lvl1, systems)
habitat_lvl1 <- mask(habitat_lvl1.1, systems)
writeRaster(habitat_lvl1, 
            here("habitat_diversity",
                 "iucn_habitatclassification_composite_lvl1_ver004_sub.tif"),
            overwrite = FALSE)


## Level 2 - raster

habitat_lvl2 <- rast(here("habitat_diversity", 
                          "iucn_habitatclassification_composite_lvl2_ver004.tif"))

habitat_lvl2 <- crop(habitat_lvl2, systems)
habitat_lvl2 <- mask(habitat_lvl2, systems)
writeRaster(habitat_lvl2, 
            here("habitat_diversity",
                 "iucn_habitatclassification_composite_lvl2_ver004_sub.tif"),
            overwrite = FALSE)


