library(terra)

# Climate Stability Index (CSI) = https://figshare.com/ndownloader/files/28170693
# Herrando-Moraira, S., Nualart, N., Galbany-Casals, M., Garcia-Jacas, N., Ohashi, 
#  H., Matsui, T., Susanna, A., Tang, C.Q., & López-Pujol, J. 2022. Climate 
#  Stability Index maps, a global high resolution cartography of climate 
#  stability from Pliocene to 2100. Sci Data 9: 48. doi: https://doi.org/10.1038/s41597-022-01144-5

csi <- terra::rast("Summary_Files/csi_past.tif")

# Galapagos
gal <- terra::vect("Galapagos/Shapefile/galapagos_island_Project.shp")
# Remove all polygons with NA name
gal_names <- gal[!is.na(gal$nombre), ]

# Ensure the polygons are gone
gal_final <- gal_names[!is.empty(gal_names), ]

# Ensure that all names are unique
gal_final$nombre <- make.unique(gal_final$nombre)

gal_islands <- gal_final

gal_islands <- subset(gal, gal$tipo == "Isla")

csi_gal <- cbind.data.frame(island = gal_islands$nombre, 
                            mean= extract(csi, gal_islands, fun=mean)[,2],
                            sd= extract(csi, gal_islands, fun=sd)[,2])
write.csv(csi_gal, "Galapagos/Data/csi.csv", row.names = FALSE)

# Hawaii
hawaii <- terra::vect("Hawaiian/Shapefile/coastline.shp")
# Remove all polygons with NA name
hawaii_names <- hawaii[!is.na(hawaii$isle), ]

# Ensure the polygons are gone
hawaii_final <- hawaii_names[!is.empty(hawaii_names), ]

# Ensure that all names are unique
hawaii_final$isle <- make.unique(hawaii_final$isle)

csi_haw <- cbind.data.frame(island = hawaii_final$isle, 
                            mean= extract(csi, hawaii_final, fun=mean)[,2],
                            sd= extract(csi, hawaii_final, fun=sd)[,2])
write.csv(csi_haw, "Hawaiian/Data/csi.csv", row.names = FALSE)


# Caribbean
caribbean <- terra::vect("Caribbean/Shapefile/caribbean_test2.shp")

# Remove all polygons with NA name
carib_names <- caribbean[!is.na(caribbean$Name_USGSO), ]

# Also remove all polygons with "UNNAMED" as a name
all_names <- carib_names$Name_USGSO
keep_names <- all_names[!all_names %in% "UNNAMED"]
# Filter to keep_names
carib_final <- carib_names[carib_names$Name_USGSO %in% keep_names, ]

# Ensure that all names are unique
carib_final$Name_USGSO <- make.unique(carib_final$Name_USGSO)

csi_car <- cbind.data.frame(island = carib_final$Name_USGSO, 
                            mean= extract(csi, carib_final, fun=mean)[,2],
                            sd= extract(csi, carib_final, fun=sd)[,2])

write.csv(csi_car, "Caribbean/Data/csi.csv", row.names = FALSE)
