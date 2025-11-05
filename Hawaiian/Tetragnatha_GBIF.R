library(rgbif)

suggestions <- name_suggest(q = "Tetragnatha", rank = "genus")

# The correct key is the first element in the first row
key <- as.numeric(suggestions$data[1,1])

download_key <- rgbif::occ_download(pred("taxonKey", key),
                                    pred("hasCoordinate", TRUE),
                                    pred("hasGeospatialIssue", FALSE),
                                    pred_within('POLYGON((-158.0 21.6, -161.6 17.6, -152.6 17.7, -152.6 23.2, -158.0 21.6))'),
                                    format = "DWCA")
dat <- rgbif::occ_download_get(download_key) %>%
  rgbif::occ_download_import()

library(ssarp)
land_dat <- find_land(dat)
area_dat <- find_areas(land_dat)
