## MAYBE WE SHOULD USE ANOVA INSTEAD??

# Visually compare SARs between target adaptive radiation and comparison

## MADAGASCAR
# Phelsuma Geckos
phel_areas <- read.csv("Data/Phelsuma_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
phel_stand <- standardize(phel_areas, 0)

# Calculate linear model with standardized data
phel_linear <- lm(y ~ x, data = phel_stand)

# Plot standardized SAR
plot(phel_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16, col = "green")
abline(phel_linear, col = "green")

# Furcifer Chameleons
furci_areas <- read.csv("Data/Furcifer_areas.csv")

# NOTE: best-fit SAR has 1 breakpoint, but it errors during the jackknife
# Using linear model instead

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
furci_stand <- standardize(furci_areas, 0)

# Calculate linear model with standardized data
furci_linear <- lm(y ~ x, data = furci_stand)

# Add furci info to the existing plot?
points(furci_stand, pch = 16, col = "blue")
abline(furci_linear, col = "blue")

legend("topleft", legend = c("Phelsuma", "Furcifer"), fill = c("green", "blue"))

## AFRICAN LAKES
# Cichlid fish
cich_areas <- read.csv("Data/Cichlid_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
cich_stand <- standardize(cich_areas, 0)

# Calculate linear model with standardized data
cich_linear <- lm(y ~ x, data = cich_stand)

# Plot standardized SAR
plot(cich_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16, col = "orange")
abline(cich_linear, col = "orange")

# Now add Enteromius info
enter_areas <- read.csv("Data/Enteromius_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
enter_stand <- standardize(enter_areas, 1) # Best-fit SAR has 1 breakpoint

# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = enter_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(enter_stand$x, enter_stand$y, pch = 19, col = "red")

# Add the cichlid info last I guess
points(cich_stand$x, cich_stand$y, pch = 19, col = "orange")
abline(cich_linear, col = "orange")

legend("topleft", legend = c("Cichlids", "Enteromius"), fill = c("orange", "red"))

## HAWAIIAN ISLANDS
# Hawaiian Silverswords
silver_areas <- read.csv("Data/Silversword_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
silver_stand <- standardize(silver_areas, 0)

# Calculate linear model with standardized data
silver_linear <- lm(y ~ x, data = silver_stand)

# Plot standardized SAR
plot(silver_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16, col = "gray")
abline(silver_linear, col = "gray")

# Hawaiian Acacia
acacia_areas <- read.csv("Data/Acacia_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
acacia_stand <- standardize(acacia_areas, 1) # Best-fit SAR has 1 breakpoint

# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = acacia_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(acacia_stand$x, acacia_stand$y, pch = 19, col = "red")

# Okay now add points for silverswords
points(silver_stand$x, silver_stand$y, pch = 19, col = "black")
abline(silver_linear, col = "black")

legend("topleft", legend = c("Silverswords", "Acacia"), fill = c("black", "red"))

## GALAPAGOS ISLANDS
# Naesiotus Snails
snail_areas <- read.csv("Data/Naesiotus_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
snail_stand <- standardize(snail_areas, 0)

# Calculate linear model with standardized data
snail_linear <- lm(y ~ x, data = snail_stand)

# Plot standardized SAR
plot(snail_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16, col = "brown")
abline(snail_linear, col = "brown")

# Galapagos Opuntia
cactus_areas <- read.csv("Data/Opuntia_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
cactus_stand <- standardize(cactus_areas, 0)

# Calculate linear model with standardized data
cactus_linear <- lm(y ~ x, data = cactus_stand)

# Add cactus info to plot
abline(cactus_linear, col = "green")
points(cactus_stand$x, cactus_stand$y, pch = 16, col = "green")

legend("topleft", legend = c("Naesiotus", "Opuntia"), fill = c("brown", "green"))


## CARIBBEAN ISLANDS
# Anolis
anole_areas <- read.csv("Data/Anolis_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
anole_stand <- standardize(anole_areas, 1)

# Plot standardized SAR
# For Anolis, this is a segmented regression
# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = anole_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(anole_stand$x, anole_stand$y, pch = 19, col = "red")

# Tropidophis
trop_areas <- read.csv("Data/Tropidophis_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
trop_stand <- standardize(trop_areas, 0)

# Calculate linear model with standardized data
trop_linear <- lm(y ~ x, data = trop_stand)

# Add points and line
points(trop_stand$x, trop_stand$y, pch = 16, col = "blue")
abline(trop_linear, col = "blue")

legend("topleft", legend = c("Anolis", "Tropidophis"), fill = c("red", "blue"))


########
# Frogs
frog_dat <- read.csv("Eleutherodactylus_dat.csv")
frog_land <- findLand(frog_dat)
# Remove USA records
frog_land <- frog_land[-which(frog_land$First == "USA"),]
frog_areas <- findAreas(frog_land)
frog_areas <- frog_areas[-which(frog_areas$areas == 1.78e13),]

frog_nocont <- removeContinents(frog_areas)
frog_nocont <- frog_nocont[-which(frog_nocont$First == "Philippines"),]

frog_SAR <- SARP(frog_nocont)

# Equivalent plot
plot(anole_SAR$segObj, rug = FALSE,
     xlim = c(12, 26),
     ylim = c(0, 5),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(anole_SAR$aggDF$x, anole_SAR$aggDF$y, pch = 19, col = "green")

plot(trop_SAR$segObj, rug = FALSE,
     xlim = c(12, 26),
     ylim = c(0, 5),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(trop_SAR$aggDF$x, trop_SAR$aggDF$y, pch = 19, col = "blue")

plot(frog_SAR$segObj, rug = FALSE,
     xlim = c(12, 26),
     ylim = c(0, 5),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(frog_SAR$aggDF$x, frog_SAR$aggDF$y, pch = 19, col = "red")

########
library(ape)
setwd("C:/Users/KMart/Documents/UArizona/SAR_Comparison")
anole_tree <- read.tree("Patton_Anolis_Trimmed.tree")
anole_sp <- speciationMS(anole_tree,label_type = "epithet", occurrences = anole_areas)
anole_SpAR <- SpeARP(anole_sp)


# Snakes
# Negative slope
trop_sp <- speciationDR(trop_tree, label_type = "epithet", trop_areas)
trop_SpAR <- SpeARP(trop_sp)

# Frogs
frog_areas$Species <- paste(frog_areas$Genus, frog_areas$Species, sep = "_")
frog_nocont$Species <- paste(frog_nocont$Genus, frog_nocont$Species, sep = "_")
frog_sp <- speciationMS(eleu_tree, label_type = "epithet", frog_nocont)
frog_SpAR <- SpeARP(frog_sp)

plot(anole_SpAR$segObj, rug = FALSE,
     xlim = c(12, 26),
     ylim = c(0, 0.07),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Speciation-Area Relationship")
# Add the points
points(anole_SpAR$aggDF$x, anole_SpAR$aggDF$y, pch = 19, col = "green")

plot(frog_SpAR$segObj, rug = FALSE,
     xlim = c(12, 26),
     ylim = c(0, 0.07),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Speciation-Area Relationship")
# Add the points
points(frog_SpAR$aggDF$x, frog_SpAR$aggDF$y, pch = 19, col = "red")


########
# Curated rain frog dataset testing
frog_dat <- read.csv("Eleutherodactylus_Curated.csv")

frog_areas <- findAreas(frog_dat)

SARP(frog_areas)

frog_tree <- read.tree("frog_tree.tree")

frog_areas$Species <- paste(frog_areas$Genus, frog_areas$Species, sep = "_")

speciation_rates <- speciationMS(frog_tree, label_type = "epithet", frog_areas)

SpeARP(speciation_rates)

# Color tips
tip_colors <- ifelse(frog_tree$tip.label %in% pr_frogs, "red", "black")

# Plot tree with colored tips
plot.phylo(frog_tree, tip.color = tip_colors, cex = 0.4)

# Okay remove Eleutherodactylus monensis for now - not actually on Puerto Rico proper
which(frog_areas$Species == "Eleutherodactylus_monensis")
frog_areas <- frog_areas[-128,]

# Try again without it
speciation_Rates <- speciationMS(frog_tree, label_type = "epithet", frog_areas)

# Eleutherodactylus auriculatus is supposed to be endemic to Cuba, but has museum records from PR
which(frog_areas$Species == "Eleutherodactylus_auriculatus")
frog_areas <- frog_areas[-25,]

# What if I pretend that schwartzi is actually on Puerto Rico
frog_areas <- rbind(frog_areas, c("Eleutherodactylus schwartzi", "Eleutherodactylus", 
                                  "Eleutherodactylus_schwartzi", NA, NA, "Puerto Rico", 
                                  NA, NA, NA, 9710687500))

# Also flavescens
frog_areas <- rbind(frog_areas, c("Eleutherodactylus flavescens", "Eleutherodactylus", 
                                  "Eleutherodactylus_flavescens", NA, NA, "Puerto Rico", 
                                  NA, NA, NA, 9710687500))



# Get the Most Recent Common Ancestor (MRCA) of the two taxa
mrca_node <- getMRCA(frog_tree, c("Eleutherodactylus_flavescens", "Eleutherodactylus_unicolor"))

# Extract all descendant taxa from this MRCA
descendants <- extract.clade(frog_tree, node = mrca_node)$tip.label

# Keep only these taxa in the tree
trimmed_tree <- keep.tip(frog_tree, descendants)

# Plot the trimmed tree
plot(trimmed_tree)

trimmed_subtrees <- subtrees(trimmed_tree)

tip_colors <- ifelse(trimmed_tree$tip.label %in% pr_frogs, "red", "black")

# Plot tree with colored tips
plot.phylo(trimmed_tree, tip.color = tip_colors, cex = 1)

# From speciationMS

# Create a df to store: each monophyletic group, the number of species in each, and the node age
mono_df <- data.frame()
for(g in seq(island_groups)){
  comp_group <- island_groups[[g]]
  # See how many subtrees are in this list of species
  for(i in seq(sub_trees)){
    # If all taxa in the current subtree is in the comparison group, add its info to the df
    if(all(sub_trees[[i]]$tip.label %in% comp_group)){
      comp_group <- g
      log_area <- names(island_groups[g])
      ntips <- sub_trees[[i]]$Ntip
      # Get node age
      # There are usually multiple nodes in the subtree, so get the root age instead?
      node_ages <- node.depth.edgelength(sub_trees[[i]])
      # The root age is the max of the node ages
      root_age <- max(node_ages)
      # Gather info to add to df
      new_row <- c(comp_group, log_area, ntips, root_age)
      mono_df <- rbind(mono_df, new_row)
    }
  }
}

comp_group <- c("Eleutherodactylus_locustus", "Eleutherodactylus_eneidae", "Eleutherodactylus_cooki")

comp_group <- c("Eleutherodactylus_cooki", "Eleutherodactylus_eneidae", "Eleutherodactylus_locustus", "Eleutherodactylus_antillensis", "Eleutherodactylus_brittoni", "Eleutherodactylus_cochranae", "Eleutherodactylus_hedricki", "Eleutherodactylus_coqui", "Eleutherodactylus_schwartzi", "Eleutherodactylus_wightmanae", "Eleutherodactylus_portoricensis", "Eleutherodactylus_gryllus", "Eleutherodactylus_flavescens")

# Find the subtrees that include all three species
matching_subtrees <- list()
for (i in seq_along(trimmed_subtrees)) {
  subtree <- trimmed_subtrees[[i]]
  if (all(comp_group %in% subtree$tip.label)) {
    matching_subtrees[[length(matching_subtrees) + 1]] <- subtree
  }
}

matching_subtrees[[3]]
ntips <- matching_subtrees[[3]]$Ntip
node_ages <- node.depth.edgelength(matching_subtrees[[3]])
root_age <- max(node_ages)
new_row <- c(NA, NA, ntips, root_age)
sp_rate <- (log(new_row[3])-log(2))/new_row[4]
sp_rate
