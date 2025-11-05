library(here)

all_files <- list.files(here(), 
                        pattern = "_final_df\\.csv$", 
                        full.names = TRUE, 
                        recursive = TRUE)
results_list <- list()

for (file_path in all_files) {
  
  # Extract lineage and archipelago
  filename <- basename(file_path)
  name_part <- sub("_final_df\\.csv$", "", filename)
  parts <- strsplit(name_part, "_")[[1]]
  
  # Extract archipelago (first part) and lineage (remaining parts)
  archipelago <- parts[1]
  lineage <- paste(parts[-1], collapse = "_")
  
  # Calculate statistics
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  n_islands <- nrow(data)
  mean_csi <- mean(data$mean_csi, na.rm = TRUE)
  mean_elevation <- mean(data$mean_elev, na.rm = TRUE)
  max_elevation <- max(data$max_elev, na.rm = TRUE)
  mean_TRI <- mean(data$TRI, na.rm = TRUE)
  max_TRI <- max(data$TRI, na.rm = TRUE)
  mean_nearest_dist <- mean(data$Nearest_Dist, na.rm = TRUE)
  max_nearest_dist <- max(data$Nearest_Dist, na.rm = TRUE)
  min_nearest_dist <- min(data$Nearest_Dist, na.rm = TRUE)
  min_area <- min(data$area, na.rm = TRUE)
  max_area <- max(data$area, na.rm = TRUE)
  mean_area <- mean(data$area, na.rm = TRUE)
  richness_nonzero <- data$richness[data$richness > 0]
  min_richness_nonzero <- min(richness_nonzero, na.rm = TRUE)
  max_richness <- max(richness_nonzero, na.rm = TRUE)
  mean_richness_nonzero <- mean(richness_nonzero, na.rm = TRUE)
  bp <- ifelse("bp" %in% colnames(data), 1, 0)
  
  # Summarize results
  result_row <- data.frame(
    archipelago = archipelago,
    lineage = lineage,
    n_islands = n_islands,
    mean_csi = mean_csi,
    mean_elevation = mean_elevation,
    max_elevation = max_elevation,
    mean_TRI = mean_TRI,
    max_TRI = max_TRI,
    mean_nearest_dist = mean_nearest_dist,
    max_nearest_dist = max_nearest_dist,
    min_nearest_dist = min_nearest_dist,
    min_area = min_area,
    max_area = max_area,
    mean_area = mean_area,
    min_richness_nonzero = min_richness_nonzero,
    max_richness = max_richness,
    mean_richness_nonzero = mean_richness_nonzero,
    bp = bp,
    stringsAsFactors = FALSE
  )
  
  results_list[[length(results_list) + 1]] <- result_row
}

# Combine all results into a single data frame
summary_matrix <- do.call(rbind, results_list)

# Merge to the trait database
traits <- read.csv(here("Summary_Files", "lineage_traits.csv"))
traits[which(traits$lineage == "finches"),1] <- "finch" 
summary_matrix <- merge(summary_matrix, traits, by = "lineage")

# Export csv file
write.csv(summary_matrix, here("Summary_Files", "AxL.csv"))
