##### Calculate isolation metrics #####
# Archipelago: Galapagos Islands
# Taxon: Galapagos Finches

library(raster)
library(terra)

# Read in shapefile from the Galapagos Geoportal (https://geodata.fcdarwin.org.ec/catalogue/#/dataset/489)
galap <- shapefile("Galapagos/Shapefile/galapagos_island_Project.shp")

# Turn it into a SpatVector
galap_v <- vect(galap)

# Remove all polygons with NA name
galap_names <- galap_v[!is.na(galap_v$nombre), ]

# Ensure the polygons are gone
galap_final <- galap_names[!is.empty(galap_names), ]

# Ensure that all names are unique
galap_final$nombre <- make.unique(galap_final$nombre)

# All names in the shapefile
all_names <- galap_final$nombre

# Remove names of islands that are smaller than the
#  smallest island with occurrence records
#  (Darwin in this case, at 664341.18 m^2)

galap_final$areas <- sf::st_area(sf::st_as_sf(galap_final))
areas_df <- as.data.frame(cbind(galap_final$nombre, galap_final$areas))
# Make sure it's numeric
areas_df$V2 <- as.numeric(areas_df$V2)

# Figure out which islands are smaller than Darwin
small <- which(areas_df$V2 < 664341)
names <- areas_df$V1[small]

final_names <- all_names[!all_names %in% names]

# Now we can filter the polygons
polygons <- terra::subset(galap_final, galap_final$nombre %in% final_names)
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
for(i in c(1:nrow(dist_cent_df))){
  # Record island numbers
  island1 <- as.numeric(dist_cent_df[i,1])
  island2 <- as.numeric(dist_cent_df[i,2])
  
  # Replace numbers with names
  dist_cent_df[i,1] <- polygons$nombre[island1]
  dist_cent_df[i,2] <- polygons$nombre[island2]
}

colnames(dist_cent_df) <- c("Island1", "Island2", "Distance_m")

## Now, find the minimum distance for each island
galapagos_dist <- dist_cent_df

# First, remove all rows with a 0 in the Distance column (the pair is with itself)
galapagos_dist <- galapagos_dist[-which(galapagos_dist$Distance_m == 0),]

# Next, find the minimum distance for each island
unique_islands <- unique(galapagos_dist$Island1)

# Create blank dataframe to store data
# Island name, min dist
dist_df <- data.frame()

# For each island in unqiue_islands, find the minimum distance
for(i in c(1:length(unique_islands))){
  rows <- which(galapagos_dist$Island1 == unique_islands[i])
  island_comps <- galapagos_dist[rows,]
  # Find closest island to the current
  min_dist <- min(island_comps$Distance_m)
  # Add to dataframe
  # Name first
  dist_df[i, 1] <- unique_islands[i]
  # Then dist
  dist_df[i, 2] <- min_dist
}

colnames(dist_df) <- c("Island", "Nearest_Dist")

write.csv(dist_df, "Galapagos/isolation_Galap_Finch.csv", row.names = FALSE)

### Now figure out which island with occurrences is closest to each island without

# Start with dist_cent_df from above

# Islands with occurrences
occ_islands <- c("Darwin", "Wolf", "Seymour Norte",
                 "Rabida", "Genovesa", "Pinzón", "Pinta",
                 "Santa Fé", "Baltra", "Española",
                 "Marchena", "Floreana", "San Cristobal",
                 "Santiago", "Fernandina", "Santa Cruz",
                 "Isabela")

# Islands without occurrences
no_occ <- final_names[!final_names %in% occ_islands]
# no_occ <- c("Cuatro Hermanos Sur", "Gardner por Floreana", "Bartolome", "Tortuga")

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
write.csv(no_occ_dists, "Galapagos/no_occ_dists_Galap_Finch.csv", row.names = FALSE)

##### Find shortest distance between only islands that have occurrences #####
dat <- read.csv("Summary_Files/IxL_distmainland.csv")
dat <- dat[which(dat$richness > 0),]
galap_dat <- dat[which(dat$archipelago == "galap"),]
finch_dat <- galap_dat[which(galap_dat$lineage == "finches"),]
rownames(finch_dat) <- c(1:length(finch_dat$name))

# Fix accents in finch_dat
all_names <- as.data.frame(all_names)
finch_dat[3,1] <- all_names[114,1]
finch_dat[10,1] <- all_names[60,1]
finch_dat[14,1] <- all_names[2,1]

# Include only islands in finch_dat
# Filter the polygons
polygons <- terra::subset(galap_final, galap_final$nombre %in% finch_dat$name)
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
for(i in c(1:nrow(dist_cent_df))){
  # Record island numbers
  island1 <- as.numeric(dist_cent_df[i,1])
  island2 <- as.numeric(dist_cent_df[i,2])
  
  # Replace numbers with names
  dist_cent_df[i,1] <- polygons$nombre[island1]
  dist_cent_df[i,2] <- polygons$nombre[island2]
}

colnames(dist_cent_df) <- c("Island1", "Island2", "Distance_m")

## Now, find the minimum distance for each island
galapagos_dist <- dist_cent_df

# First, remove all rows with a 0 in the Distance column (the pair is with itself)
galapagos_dist <- galapagos_dist[-which(galapagos_dist$Distance_m == 0),]

# Next, find the minimum distance for each island
unique_islands <- unique(galapagos_dist$Island1)

# Create blank dataframe to store data
# Island name, min dist
dist_df <- data.frame()

# For each island in unqiue_islands, find the minimum distance
for(i in c(1:length(unique_islands))){
  rows <- which(galapagos_dist$Island1 == unique_islands[i])
  island_comps <- galapagos_dist[rows,]
  # Find closest island to the current
  min_dist <- min(island_comps$Distance_m)
  # Add to dataframe
  # Name first
  dist_df[i, 1] <- unique_islands[i]
  # Then dist
  dist_df[i, 2] <- min_dist
}

colnames(dist_df) <- c("Island", "Nearest_Dist")

# Write new distance data
write.csv(dist_df, "Galapagos/Data/distance_only_occs_islands_galap_finches.csv", row.names = FALSE)

##### Find distance from an island with an occurrence to another island with an occurrence #####
# Read in galap_final from Galapagos_Elevation.R
galap_final <- readRDS("Galapagos/Data/galap_final.rds")
# Convert to terra vector
galap_final <- vect(galap_final)

# Read in IxL dataset (the basis for everything)
ixl <- read.csv("ixl_WIP.csv")

# Filter to finches
dat <- ixl[which(ixl$lineage == "finches"),]
# Include only islands that have an occurrence on them
dat <- dat[which(dat$richness > 0),]

# Fix the names
source("FixNames.R")
dat <- fixnames_galap(dat)
ixl <- fixnames_galap(ixl)

# Now we can filter the polygons we're not using
polygons <- terra::subset(galap_final, galap_final$nombre %in% dat$name)
# Ensure the unwanted polygons are gone
polygons <- polygons[!is.empty(polygons), ]

# Terra has a centroids function
cent <- centroids(polygons)
# Get distance between all centroids
dist_cent <- terra::distance(cent)

# Transform distance matrix into something more interpetable
library(reshape2)
dist_cent_df <- melt(as.matrix(dist_cent), varnames = c("row", "col"))
dist_cent_df[,1] <- polygons$nombre[as.numeric(dist_cent_df[,1])]
dist_cent_df[,2] <- polygons$nombre[as.numeric(dist_cent_df[,2])]

colnames(dist_cent_df) <- c("Island1", "Island2", "Distance_m")

## Now, find the minimum distance for each island
galap_dist <- dist_cent_df

# First, remove all rows with a 0 in the Distance column (the pair is with itself)
galap_dist <- galap_dist[-which(galap_dist$Distance_m == 0),]

# Next, find the minimum distance for each island
unique_islands <- unique(galap_dist$Island1)

# Create blank dataframe to store data
# Island name, min dist
dist_df <- data.frame()

# For each island in unqiue_islands, find the minimum distance
for(i in c(1:length(unique_islands))){
  rows <- which(galap_dist$Island1 == unique_islands[i])
  island_comps <- galap_dist[rows,]
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
      # check if it's a finch row
      if(ixl$lineage[row] == "finches"){
        # Then add the distance between occ islands
        ixl$dist_occ_islands[row] <- dist_df[i,2]
      }
    }
  }
}
