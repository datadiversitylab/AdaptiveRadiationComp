##### Calculate isolation metrics #####
# Archipelago: Caribbean Islands
# Taxon: Eleutherodactylus frogs

library(raster)
library(terra)

# Read in shapefile cropped from the new USGS global island dataset
# (https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273)
carib <- shapefile("Caribbean/Shapefile/caribbean_test2.shp")

# Turn it into a SpatVector
carib_v <- vect(carib)

# Remove all polygons with NA name
carib_names <- carib_v[!is.na(carib_v$Name_USGSO), ]

# Also remove all polygons with "UNNAMED" as a name
all_names <- carib_names$Name_USGSO
keep_names <- all_names[!all_names %in% "UNNAMED"]
# Filter to keep_names
carib_final <- carib_names[carib_names$Name_USGSO %in% keep_names, ]

# Ensure that all names are unique
carib_final$Name_USGSO <- make.unique(carib_final$Name_USGSO)

# All names in the shapefile
all_names <- carib_final$Name_USGSO

# Remove names of islands that are smaller than the
#  smallest island with occurrence records
#  (Long Island in this case, at 1272350 m^2)

carib_final$areas <- sf::st_area(sf::st_as_sf(carib_final))
areas_df <- as.data.frame(cbind(carib_final$Name_USGSO, carib_final$areas))
# Make sure it's numeric
areas_df$V2 <- as.numeric(areas_df$V2)

# Figure out which islands are smaller than Long Island
small <- which(areas_df$V2 < 1272350)
names <- areas_df$V1[small]

final_names <- all_names[!all_names %in% names]

# Now we can filter the polygons
polygons <- terra::subset(carib_final, carib_final$Name_USGSO %in% final_names)
# Ensure the unwanted polygons are gone
polygons <- polygons[!is.empty(polygons), ]

# Terra has a centroids function
cent <- centroids(polygons)
# Get distance between all centroids
dist_cent <- terra::distance(cent)

## Transform distance matrix into something more interpetable
library(reshape2)
dist_cent_df <- melt(as.matrix(dist_cent), varnames = c("row", "col"))

# Each row and column corresponds to the island in that position
# for(i in c(1:nrow(dist_cent_df))){
#   # Record island numbers
#   island1 <- as.numeric(dist_cent_df[i,1])
#   island2 <- as.numeric(dist_cent_df[i,2])
#   
#   # Replace numbers with names
#   dist_cent_df[i,1] <- polygons$Name_USGSO[island1]
#   dist_cent_df[i,2] <- polygons$Name_USGSO[island2]
# }

# The loop takes too long, maybe do this instead?
dist_cent_df[,1] <- polygons$Name_USGSO[as.numeric(dist_cent_df[,1])]
dist_cent_df[,2] <- polygons$Name_USGSO[as.numeric(dist_cent_df[,2])]

colnames(dist_cent_df) <- c("Island1", "Island2", "Distance_m")

## Now, find the minimum distance for each island
carib_dist <- dist_cent_df

# First, remove all rows with a 0 in the Distance column (the pair is with itself)
carib_dist <- carib_dist[-which(carib_dist$Distance_m == 0),]

# Next, find the minimum distance for each island
unique_islands <- unique(carib_dist$Island1)

# Create blank dataframe to store data
# Island name, min dist
dist_df <- data.frame()

# For each island in unqiue_islands, find the minimum distance
for(i in c(1:length(unique_islands))){
  rows <- which(carib_dist$Island1 == unique_islands[i])
  island_comps <- carib_dist[rows,]
  # Find closest island to the current
  min_dist <- min(island_comps$Distance_m)
  # Add to dataframe
  # Name first
  dist_df[i, 1] <- unique_islands[i]
  # Then dist
  dist_df[i, 2] <- min_dist
}

colnames(dist_df) <- c("Island", "Nearest_Dist")

write.csv(dist_df, "Caribbean/isolation_Carib_Frogs.csv", row.names = FALSE)

### Now figure out which island with occurrences is closest to each island without

# Start with dist_cent_df from above

# Figure out which islands have occurrences
frog_df <- read.csv("Caribbean/Data/Eleutherodactylus_Curated.csv")
frog_islands <- frog_df$First

unique_islands <- unique(frog_islands)
occ_df <- areas_df[areas_df$V1 %in% unique_islands,]

# Islands with occurrences
occ_islands <- occ_df$V1

# Islands without occurrences
no_occ <- final_names[!final_names %in% occ_islands]

# For each island with no occurrences, find the closest island with occurrences
# First: select just rows with the no occurrence island as Island2
# Next: select just rows with occurrence islands as Island1
# Finally: get the minimum distance

no_occ_dists <- data.frame()

for(i in c(1:length(no_occ))){
  no_occ_island <- no_occ[i]
  no_occ_rows <- dist_cent_df[which(dist_cent_df$Island2 == no_occ_island),]
  
  occ_rows <- no_occ_rows[which(no_occ_rows$Island1 %in% occ_islands),]
  
  no_occ_dists[i,1] <- no_occ_island
  no_occ_dists[i,2] <- min(occ_rows$Distance_m)
}

colnames(no_occ_dists) <- c("Island", "Distance_m")
write.csv(no_occ_dists, "Caribbean/no_occ_dists_Carib_Frogs.csv", row.names = FALSE)

##### Find distance from an island with an occurrence to another island with an occurrence #####
# Read in carib_final from Caribbean_Elevation.R
carib_final <- readRDS("Caribbean/Data/carib_final.rds")
# Convert to terra vector
carib_final <- vect(carib_final)

# Read in IxL dataset (the basis for everything)
ixl <- read.csv("Summary_Files/IxL.csv")

# Filter to eleutherodactylus
dat <- ixl[which(ixl$lineage == "eleutherodactylus"),]
# Include only islands that have an occurrence on them
dat <- dat[which(dat$richness > 0),]

# Fix the names
source("FixNames.R")
dat <- fixnames_carib(dat)
ixl <- fixnames_carib(ixl)

# Now we can filter the polygons we're not using
polygons <- terra::subset(carib_final, carib_final$Name_USGSO %in% dat$name)
# Ensure the unwanted polygons are gone
polygons <- polygons[!is.empty(polygons), ]

# Terra has a centroids function
cent <- centroids(polygons)
# Get distance between all centroids
dist_cent <- terra::distance(cent)

# Transform distance matrix into something more interpetable
library(reshape2)
dist_cent_df <- melt(as.matrix(dist_cent), varnames = c("row", "col"))
dist_cent_df[,1] <- polygons$Name_USGSO[as.numeric(dist_cent_df[,1])]
dist_cent_df[,2] <- polygons$Name_USGSO[as.numeric(dist_cent_df[,2])]

colnames(dist_cent_df) <- c("Island1", "Island2", "Distance_m")

## Now, find the minimum distance for each island
carib_dist <- dist_cent_df

# First, remove all rows with a 0 in the Distance column (the pair is with itself)
carib_dist <- carib_dist[-which(carib_dist$Distance_m == 0),]

# Next, find the minimum distance for each island
unique_islands <- unique(carib_dist$Island1)

# Create blank dataframe to store data
# Island name, min dist
dist_df <- data.frame()

# For each island in unqiue_islands, find the minimum distance
for(i in c(1:length(unique_islands))){
  rows <- which(carib_dist$Island1 == unique_islands[i])
  island_comps <- carib_dist[rows,]
  # Find closest island to the current
  min_dist <- min(island_comps$Distance_m)
  # Add to dataframe
  # Name first
  dist_df[i, 1] <- unique_islands[i]
  # Then dist
  dist_df[i, 2] <- min_dist
}

colnames(dist_df) <- c("Island", "Nearest_Dist")

# Add column to IxL for distance between occ islands
# (Already added in Anolis script)
# ixl$dist_occ_islands <- rep(NA, length(ixl$name))

for(i in c(1:nrow(dist_df))){
  match_rows <- which(ixl$name == dist_df[i,1])
  if(length(match_rows) != 0){
    # For each row in match_rows,
    for(row in match_rows){
      # check if it's a frog row
      if(ixl$lineage[row] == "eleutherodactylus"){
        # Then add the distance between occ islands
        ixl$dist_occ_islands[row] <- dist_df[i,2]
      }
    }
  }
}
