library(rgbif)
library(dplyr)

setwd("~/GitHub/AdaptiveRadiationComp/Caribbean")

download_key <- readRDS("download_key.rds")

dat <- rgbif::occ_download_get(download_key) %>%
  rgbif::occ_download_import()

occs <- ssarp::find_land(dat)

# Remove records that don't have a genus/species split
occs <- occs[which(occs$genericName != ""),]
write.csv(occs, "occs_gbif.csv", row.names = FALSE)

occs <- read.csv("occs_gbif.csv")

occs$species <- paste(occs$genericName, occs$specificEpithet)

library(CoordinateCleaner)
rl <- clean_coordinates(occs)
summary(rl)
# If the cell is FALSE, that means that it failed the check
# So if you want to remove all the ones that have a sea problem, then
#  you remove these:
bloop <- which(rl$.sea == FALSE)

# How does that compare to the records that don't have any data from ssarp?
blarp <- which(is.na(rl$first))

# There are fewer that have problems when using ssarp::find_land

# Remove records that did not make it through find_land()
occs_land <- occs[which(!is.na(occs$first)),]

# Get areas
occs_areas <- find_areas(occs_land)

write.csv(occs_areas, "Anolis_occs_areas.csv", row.names = FALSE)

# Create CSV for derived dataset
occs_areas <- occs_areas[,-10]
occs_areas <- occs_areas[,-10]
occs_areas <- occs_areas[,-6]
occs_areas <- occs_areas[,-6]
occs_areas <- occs_areas[,-6]

# Remove records that weren't used
occs_areas <- occs_areas[which(occs_areas$genericName != "Chamaeleolis"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Chamaelinorops"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Dactyloa"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Deiroptyx"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Norops"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Placopsis"),]
occs_areas <- occs_areas[which(occs_areas$genericName != "Xiphocercus"),]

# Write CSV
write.csv(occs_areas, "Anolis_derived_dataset.csv", row.names = FALSE)

# Create PAM?
occs_pam <- get_presence_absence(occs_areas)

# Actually, what's different between this and the one I used before?
# island | species | area
occs_test <- occs_areas
occs_test$species <- paste(occs_test$genericName, occs_test$specificEpithet)

island <- rep(NA, length(occs_test$species))
for(i in c(1:78799)){
  if(!is.na(occs_test$third[i])){
    # If third is populated, that's the island
    island[i] <- occs_test$third[i]
  } else if (!is.na(occs_test$second[i])){
    # If third is NA but second is populated, that's the island
    island[i] <- occs_test$second[i]
  } else {
    island[i] <- occs_test$first[i]
  }
}

occs_test$island <- island

occs_test <- cbind(occs_test$island, occs_test$species, occs_test$areas)
colnames(occs_test) <- c("island", "species", "areas")
bloop <- unique(occs_test)

write.csv(bloop, "anole_filter_testing.csv", row.names = FALSE)

library(letsRept)
anole_search <- reptSearch(binomial = "Anolis higuey")

# REMOVE FROM DERIVED DATASET:
# Chamaeleolis barbatus
# Chamaeleolis chamaeleonides
# Chamaeleolis guamuhaya
# Chamaeleolis porcus
# Chamaelinorops barbouri
# Chamaelinorops wetmorei
# Dactyloa roquet
# Deiroptyx vermiculata
# Norops allogus
# Norops homolechis
# Norops jubar
# Norops sagrei
# Placopsis ocellata
# Xiphocercus valenciennesi

dat <- read.csv("Data/Anolis_derived_dataset.csv")
duplicated(dat)
dat_nd <- dat[!duplicated(dat),]

write.csv(dat_nd, "Data/Anolis_DD.csv", row.names = FALSE)
source_df <- get_sources(dat_nd)
write.csv(source_df, "Data/Anolis_Sources.csv", row.names = FALSE)
