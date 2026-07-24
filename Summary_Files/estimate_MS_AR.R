

estimate_MS_AR <- function(tree, label_type = "binomial", occurrences, epsilon) {

  # Get all subtrees from given phylogenetic tree
  sub_trees <- subtrees(tree)

  # If the user specified label_type = "epithet"
  if (label_type == "epithet") {
    # Organize species into island groups
    island_groups <- tapply(occurrences$specificEpithet, occurrences$areas, unique)
  } else if (label_type == "binomial") {
    # The occurrence record dataframe has separate "genus" and "species" columns, so they should be combined for this label type
    # First, double-check that the "Species" column doesn't have any NAs
    occurrences <- occurrences[!is.na(occurrences$specificEpithet), ]
    # Then create a new column with the full name
    occurrences$Binomial <- paste(
      occurrences$Genus,
      occurrences$specificEpithet,
      sep = "_"
    )
    # Organize species into island groups
    island_groups <- tapply(occurrences$Binomial, occurrences$areas, unique)
  }

  # Create a df to store for each monophyletic group: 
  #  island area, rate, and number of tips
  mono_df <- data.frame()
  
  for (g in seq(island_groups)) {
    comp_group <- island_groups[[g]]
    
    # Create a list of candidate subtrees for this group of species
    candidate_subtrees <- list()
  
    # See how many subtrees are in this list of species
    for (i in seq(sub_trees)) {
      # Skip singleton clades (just in case it's possible)
      if(sub_trees[[i]]$Ntip < 2){
        next
      }
      
      # If all taxa in the current subtree is in the comparison group,
      #  save it as a candidate for the largest
      if (all(sub_trees[[i]]$tip.label %in% comp_group)) {
        print("all in")
        # Add to the next spot in the list
        candidate_subtrees[[length(candidate_subtrees) + 1]] <- sub_trees[[i]]
      }
    }
    
    # Find the largest candidate subtree for this island group
    sizes <- sapply(candidate_subtrees, function(x) x$Ntip)
    
    if(length(sizes) == 0){
      rate <- 0
      print("rate is zero")
      next
    } else {
      candidate_subtrees <- candidate_subtrees[sizes == max(sizes)]
      
      # Estimate rates
      for(clade in candidate_subtrees){
        # Get the length from present to the MRCA for the clade
        time <- max(node.depth.edgelength(clade))
        
        rate <- as.numeric(geiger::bd.ms(n = clade$Ntip,
                                         time = time,
                                         epsilon = epsilon))
        
        island_area <- names(island_groups[g])
        new_row <- c(island_area, rate, clade$Ntip)
        mono_df <- rbind(mono_df, new_row)
      }
    }
    
  }

  colnames(mono_df) <- c("area", "rate", "Ntip")
  
  # Ensure that the columns are numeric
  mono_df$area <- as.numeric(mono_df$area)
  mono_df$rate <- as.numeric(mono_df$rate)
  mono_df$Ntip <- as.numeric(mono_df$Ntip)

  # Get average of speciation rates for islands with multiple subtrees
  sp_rate_df <- data.frame()
  uniq_sub <- unique(mono_df$area)
  for (island in uniq_sub) {
    rows <- which(mono_df$area == island)
    sp_rate <- mean(mono_df[rows, 2])
    sp_rate_df <- rbind(sp_rate_df, c(island, sp_rate))
  }
  colnames(sp_rate_df) <- c("area", "rate")

  # Now create full dataframe
  # Since we're looking at speciation rates for each island, we don't need to retain species names
  uniq_islands <- unique(occurrences$areas)
  # If an island only had one species on it, the speciation rate will remain zero
  sp_rates <- rep(0, length(uniq_islands))
  final_df <- as.data.frame(cbind(uniq_islands, sp_rates))

  # Add speciation rates for specific islands from sp_rate_df
  for (i in 1:length(sp_rate_df$area)) {
    # Figure out which row has the current area
    ind <- which(final_df$uniq_islands == sp_rate_df[i, 1])
    # Add corresponding speciation rate to final_df
    final_df[ind, 2] <- sp_rate_df[i, 2]
  }

  # Remove rows with NA in area column
  final_df <- final_df[!is.na(final_df$uniq_islands), ]

  # Rename columns
  colnames(final_df) <- c("areas", "rate")

  # Ensure columns are numeric
  final_df$areas <- as.numeric(final_df$areas)
  final_df$rate <- as.numeric(final_df$rate)

  # The rates here are logged, which would make it incorrect to log them again
  #   when the speciation-area relationship is plotted (as happens in ssarp::create_spar).
  # To this end, we will exponentiate the rate values here so when they are logged
  #   in ssarp::create_spar, the rates will be displayed appropriately.
  final_df$rate <- exp(final_df$rate)

  return(final_df)
}
