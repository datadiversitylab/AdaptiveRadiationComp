library(here)
library(terra)

# CSI = https://figshare.com/ndownloader/files/28170693
# This needs to be stored in a folder named `past` in the repo

csi <- terra::rast(here("CSI", "csi_past.tif"))

# Galapagos
gal <- terra::vect(here("Galapagos", "Shapefile" ,"galapagos_island_Project.shp"))
gal_islands <- subset(gal, gal$tipo == "Isla")
csi_gal <- cbind.data.frame(island = gal_islands$nombre, 
                            mean= extract(csi, gal_islands, fun=mean)[,2],
                            sd= extract(csi, gal_islands, fun=sd)[,2])
write.csv(csi_gal, here("CSI", "Galapagos.csi.csv"))

# Hawaii
hawaii <- terra::vect(here("Hawaiian", "Shapefile" ,"coastline.shp"))
# Remove all polygons with NA name
hawaii_names <- hawaii[!is.na(hawaii$isle), ]

# Ensure the polygons are gone
hawaii_final <- hawaii_names[!is.empty(hawaii_names), ]

# Ensure that all names are unique
hawaii_final$isle <- make.unique(hawaii_final$isle)

csi_haw <- cbind.data.frame(island = hawaii_final$isle, 
                            mean= extract(csi, hawaii_final, fun=mean)[,2],
                            sd= extract(csi, hawaii_final, fun=sd)[,2])
write.csv(csi_haw, here("CSI", "Hawaii.csi.csv"))


# Caribbean
caribbean <- terra::vect(here("Caribbean", "Shapefile" ,"caribbean_test2.shp"))

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

write.csv(csi_car, here("CSI", "Caribbean.csi.csv"))





