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
csi_haw <- cbind.data.frame(island = hawaii$isle, 
                            mean= extract(csi, hawaii, fun=mean)[,2],
                            sd= extract(csi, hawaii, fun=sd)[,2])
write.csv(csi_haw, here("CSI", "Hawaii.csi.csv"))


# Caribbean
caribbean <- terra::vect(here("Caribbean", "Shapefile" ,"caribbean_test2.shp"))
csi_car <- cbind.data.frame(island = caribbean$Name_USGSO, 
                            mean= extract(csi, caribbean, fun=mean)[,2],
                            sd= extract(csi, caribbean, fun=sd)[,2])
write.csv(csi_car, here("CSI", "Caribbean.csi.csv"))





