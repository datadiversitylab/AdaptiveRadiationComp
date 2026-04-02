ixl <- read.csv("Summary_Files/IxL.csv")
galap_dist <- read.csv("Galapagos/Data/distance_to_mainland_GalapIslands.csv")
hawaii_dist <- read.csv("Hawaiian/Data/distance_to_mainland_HawaiianIslands.csv")
carib_dist <- read.csv("Caribbean/Data/distance_to_mainland_CaribIslands_corrupt.csv")

# Add a column for distance to mainland
ixl$dist_mainland <- rep(0, nrow(ixl))

# Galapagos #
for(i in c(1:nrow(galap_dist))){
  match_rows <- which(ixl$name == galap_dist[i,2])
  if(length(match_rows) != 0){
    ixl$dist_mainland[match_rows] <- galap_dist[i,3]
  }
}

# Problem names (accents messed up)
match_rows <- which(ixl$name == ixl[17,1])
ixl$dist_mainland[match_rows] <- galap_dist[2,3]

# Hawaii #
for(i in c(1:nrow(hawaii_dist))){
  match_rows <- which(ixl$name == hawaii_dist[i,2])
  if(length(match_rows) != 0){
    ixl$dist_mainland[match_rows] <- hawaii_dist[i,3]
  }
}

# Caribbean #
for(i in c(1:nrow(carib_dist))){
  match_rows <- which(ixl$name == carib_dist[i,1])
  if(length(match_rows) != 0){
    ixl$dist_mainland[match_rows] <- carib_dist[i,2]
  }
}

no_dist <- which(ixl$dist_mainland == 0)
no_dist_df <- ixl[no_dist,]
no_dist_names <- unique(no_dist_df$name)

# Problem names (accents messed up)
match_rows <- which(ixl$name == ixl[767,1])
ixl$dist_mainland[match_rows] <- carib_dist[291,2]

# Write final csv
write.csv(ixl, "Summary_Files/IxL_distmainland.csv")
