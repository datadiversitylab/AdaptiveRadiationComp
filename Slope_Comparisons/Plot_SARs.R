# Generating SARs and SpARs for the taxa of interest
# NOTE: Assumes that the working directory is the root of the GitHub repo

# Load libraries
library(ssarp)
library(Dict)

##### Caribbean Anolis #####
# Anole occurrences with native/nonnative designation
anole_dat <- read.csv("Caribbean/anole_filter_testing.csv")

# Include only natively occurring records
anole_native <- anole_dat[which(anole_dat$native == 1),]

# Rename species column for use with ssarp::create_SAR
colnames(anole_native)[2] <- "specificEpithet"

# Infer SAR
anole_SAR <- create_SAR(occurrences = anole_native, npsi = 2, visualize = TRUE)
confint(anole_SAR$segObj)
#        Est.        CI(95%).low CI(95%).up
# psi1.x 22.1565     21.3179     22.995

##### Caribbean Eleutherodactylus #####
# Curated rain frog occurrences
frog_dat <- read.csv("Caribbean/Data/Eleutherodactylus_Curated.csv")

# Associate occurrences with areas using a chunk from ssarp::find_areas()
# Add a temporary key-value pair to initialize
island_dict <- Dict::Dict$new(
  bloop = 108
)

# For each island name in the current dataframe,
# find the area and add the pair to the dictionary

# First, create an empty list of island names
islands <- list()

# Next, go through the occs dataframe
# All island names are in "First" column
for (i in seq_len(nrow(frog_dat))) {
  if (!is.na(frog_dat[i, "First"])) {
    islands[i] <- frog_dat[i, "First"]
  }
}

# Next, eliminate duplicate entries in the list
uniq_islands <- unique(islands)

# Next, add the island names as keys and their corresponding areas as values
area_file <- get_island_areas()

# Look through the island area file and find the names in uniq_islands list
# Initialize vector of island names from island area dataset with
#  "Island" appended
area_file_append <- paste0(area_file$Name, " Island")
# Initialize grep statements as NA
grep_res <- grep_res2 <- grep_res3 <- NA

for (i in seq(uniq_islands)) {
  # Use grep for exact match in the area database
  # [1] picks the first match if the query gets multiple matches
  query <- paste0("^", as.character(uniq_islands[i]), "$")
  grep_res <- grep(query, area_file$Name)[1]
  
  if (!is.na(grep_res)) {
    # If grep found a match, add it to island dictionary
    island_dict[as.character(uniq_islands[i])] <- area_file[
      grep_res,
      "AREA"
    ]
  } else {
    # If it doesn't find the name directly from uniq_islands, try adding
    #  "island" at the end
    query <- paste0("^", as.character(uniq_islands[i]), " Island$")
    grep_res2 <- grep(query, area_file$Name)[1]
    if (!is.na(grep_res2)) {
      # If grep found a match, add it to island dictionary
      island_dict[as.character(uniq_islands[i])] <- area_file[
        grep_res2,
        "AREA"
      ]
    }
  }
  
  # If it doesn't find the name from uniq_islands, look in area_file_append
  if (is.na(grep_res2)) {
    query <- paste0("^", as.character(uniq_islands[i]), "$")
    grep_res3 <- grep(query, area_file_append)[1]
    if (!is.na(grep_res3)) {
      # If grep found a match, add it to island dictionary
      island_dict[as.character(uniq_islands[i])] <- area_file[
        grep_res3,
        "AREA"
      ]
    }
  }
}

# Use the dictionary to add the areas to the final dataframe
areas <- rep(0, times = nrow(frog_dat))

for (i in seq_len(nrow(frog_dat))) {
  if (
    !is.na(frog_dat[i, "First"]) && island_dict$has(frog_dat[i, "First"])
  ) {
    areas[i] <- island_dict$get(frog_dat[i, "First"])
  } else {
    areas[i] <- NA
  }
}

# Create final dataframe
occs_final <- cbind(frog_dat, areas)
# Rename Species column to specificEpithet
colnames(occs_final)[3] <- "specificEpithet"

# Infer SAR
frog_SAR <- create_SAR(occs_final, npsi = 1, visualize = TRUE)
print(frog_SAR)
confint(frog_SAR$segObj)
#           Est.    CI(95%).low CI(95%).up
# psi1.x 21.1851     19.7247    22.6454

##### Hawaiian Tetragnatha #####
spider_dat <- read.csv("Hawaiian/Data/Tetragnatha_Areas.csv")
colnames(spider_dat)[3] <- "specificEpithet"
spider_SAR <- create_SAR(spider_dat, npsi = 1, visualize = TRUE)
spider_SAR$summary

##### Hawaiian Silverswords #####
silver_dat <- read.csv("Hawaiian/Data/Silversword_Areas.csv")
colnames(silver_dat)[2] <- "specificEpithet"
silver_SAR <- create_SAR(silver_dat, npsi = 1, visualize = TRUE)
confint(silver_SAR$segObj)

##### Galapagos finches #####
finch_dat <- read.csv("Galapagos/Data/finch_pam_areas_2.csv")
finch_SAR <- create_SAR(finch_dat, npsi = 1, visualize = TRUE)

##### Galapagos Scalesia #####
scal_dat <- read.csv("Galapagos/Data/Scalesia_Areas.csv")
colnames(scal_dat)[3] <- "specificEpithet"
scal_SAR <- create_SAR(scal_dat, npsi = 1, visualize = TRUE)
confint(scal_SAR$segObj)


# Save
saveRDS(anole_SAR, "Caribbean/Data/anole_SAR.rds")
saveRDS(frog_SAR, "Caribbean/Data/frog_SAR.rds")
saveRDS(finch_SAR, "Galapagos/Data/finch_SAR.rds")
saveRDS(scal_SAR, "Galapagos/Data/scal_SAR.rds")
saveRDS(spider_SAR, "Hawaiian/Data/spider_SAR.rds")
saveRDS(silver_SAR, "Hawaiian/Data/silver_SAR.rds")