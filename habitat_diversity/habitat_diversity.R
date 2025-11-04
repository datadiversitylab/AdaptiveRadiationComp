library(terra)
library(sf)
library(dplyr)
library(here)

#https://zenodo.org/records/4058819
habitat_lvl1 <- rast(here("habitat_diversity", 
                          "iucn_habitatclassification_composite_lvl1_ver004.tif"))

# HawaiianAsteraceae (from IUCN; geo [hawaii st.] and taxonomic [asteraceae] restriction)
ranges <- vect(here("habitat_diversity", "HawaiianAsteraceae/data_0.shp"))
extracted_values <- extract(habitat_lvl1, ranges)
hw <- length(unique(extracted_values[,2]))

# GalapagosFinches (from IUCN; geo [galapagos] and taxonomic [thraupidae] restriction)
ranges <- vect(here("habitat_diversity", "CaribbeanAnoles/data_0.shp"))
extracted_values <- extract(habitat_lvl1, ranges)
gl <- length(unique(extracted_values[,2]))

# CaribbeanAnoles (from IUCN; geo [caribbean] and taxonomic [anolis] restriction)
ranges <- vect(here("habitat_diversity", "CaribbeanAnoles/data_0.shp"))
extracted_values <- extract(habitat_lvl1, ranges)
ca <- length(unique(extracted_values[,2]))

# CaribbeanEleuterodactylus (from IUCN; geo [caribbean] and taxonomic [eleuterodactylus] restriction)
ranges <- vect(here("habitat_diversity", "CaribbeanEleuterodactylus/data_0.shp"))
extracted_values <- extract(habitat_lvl1, ranges)
ce <- length(unique(extracted_values[,2]))

# Scalesia (ranges from BIEN)
library(BIEN)
temp_dir <- file.path(tempdir(), "BIEN_temp")
ranges <- BIEN_ranges_genus("Scalesia", directory = temp_dir)
ranges <- lapply(ranges[,1], function(x) read_sf(dsn = temp_dir,layer = x))
extracted_values <- lapply(ranges, function(x) extract(habitat_lvl1, x))
extracted_values <- do.call(rbind, extracted_values)
sc <- length(unique(extracted_values[,2]))


#Tetragnatha (buffer around gbif occurrences)
library(rgbif)
tetragnatha_key <- name_backbone("Tetragnatha")$usageKey
tetragnatha_data <- occ_search(
  taxonKey = tetragnatha_key,
  country = "US",
  stateProvince = "Hawaii",
  hasCoordinate = TRUE
)
occur_data <- tetragnatha_data$data
occur_clean <- occur_data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  filter(coordinateUncertaintyInMeters < 100 | is.na(coordinateUncertaintyInMeters)) # Optional: filter by uncertainty

occur_points <- vect(occur_clean, 
                     geom = c("decimalLongitude", "decimalLatitude"),
                     crs = "EPSG:4326")

occur_buffers <- buffer(occur_points, width = 10) #10 meters buffer
extracted_values <- extract(habitat_lvl1, occur_buffers, fun = mean, na.rm = TRUE)
tg <- length(unique(extracted_values[,2]))

# Export dataset
hd <- rbind(
"Tetragnatha" = tg,
"Scalesia" = sc,
"Caribbean Eleuterodactylus" = ce,
"Caribbean Anoles" = ca,
"Galapagos Finches" = gl,
"Hawaiian Asteraceae" = hw
)

colnames(hd) <- "n_habitats"
write.csv(here("habitat_diversity", "habitat.diversity.csv"))



