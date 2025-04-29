##### Caribbean Adaptive Radiation Comparisons #####
# Anolis lizards (adaptive radiation)
# Eleutherodactylus frogs (adaptive radiation)
# Tropidophis snakes (not a radiation)

# Load libraries
library(ape)
library(SSARP)
library(checkmate) # For speciationMS2

# Load speciationMS2
source("../speciationMS2.R")

##### Anolis #####
# This file was created as part of Chapter 2 in: Martinet, K. (2024). 
#  Video Games and Web Databases as Novel Computational Tools for 
#  Understanding Biodiversity. University of Idaho ProQuest Dissertations & Theses.
#  Available from: https://www.proquest.com/docview/3058590063?pq-origsite=primo
anole_dat <- read.csv("Data/Anolis_areas.csv")

# Create species-area relationship (SAR)
SARP(occurrences = anole_dat, npsi = 1)
# 1 breakpoint at 22.293

# Read in tree for speciation-area relationship (SpAR)
# Tree from: Patton, A.H., Harmon, L.J., del Rosario Castañeda, M., Frank, 
#  H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When adaptive 
#  radiations collide: Different evolutionary trajectories between and within 
#  island and mainland lizard clades. PNAS, 118(42): e2024451118
#  Trimmed to include only Caribbean species
anole_tree <- read.tree("Data/Patton_Anolis_Trimmed.tree")

# Calculate speciation rates
anole_speciation <- speciationMS2(tree = anole_tree, label_type = "epithet", occurrences = anole_dat)

# Plot SpAR
SpeARP(anole_speciation, npsi = 1)
# 1 breakpoint at 22.263

##### Eleutherodactylus #####
# key <- getKey("Eleutherodactylus", "genus")
# dat <- getData(key, limit = 100000)
# land <- findLand(dat)
# write.csv(land, "Data/Eleutherodactylus_dat.csv", row.names = FALSE)
# NOTE: the above CSV was manually curated to only include native occurrence records

# The curated CSV is used in this analysis:
frog_dat <- read.csv("Data/Eleutherodactylus_Curated.csv")

# Get areas of islands in frog_dat
frog_areas <- findAreas(frog_dat)

# Plot SAR
SARP(occurrences = frog_areas, npsi = 1)
# 1 breakpoint at 21.114

# Read in tree for SpAR
# Tree from: Portik, D.M, Streicher, J.W., & Wiens, J.J. (2023). Frog phylogeny:
#  A time-calibrated, species-level tree based on hundreds of loci and 5,242
#  species. Molecular Phylogenetics and Evolution, 188: 107907. DOI: https://doi.org/10.1016/j.ympev.2023.107907

# frog_tree <- read.tree("TreePL-Rooted_Anura_bestTree.tre")
# frog_sp <- frog_tree$tip.label[grep("Eleutherodactylus", frog_tree$tip.label)]

# # These are the tips to drop from the tree
# drop_sp <- setdiff(frog_tree$tip.label, frog_sp)
# 
# # Drop those tips from the tree
# eleu_tree <- ape::drop.tip(frog_tree, drop_sp)
# 
# # Write tree to file
# write.tree(eleu_tree, "frog_tree.tree")

# Read in trimmed tree
frog_tree <- read.tree("Data/frog_tree.tree")

# Create combined species column for speciationMS
frog_areas$Species <- paste(frog_areas$Genus, frog_areas$Species, sep = "_")

# Calculate speciation rates
speciation_rates <- speciationMS2(frog_tree, label_type = "epithet", frog_areas)

# Plot SpAR
SpeARP(speciation_rates, npsi = 1)
# 1 breakpoint at 21.7

##### Tropidophis #####
# key <- getKey("Tropidophis", "genus")
# dat <- getData(key, limit = 100000)
# land <- findLand(dat)
# area <- findAreas(land)
# write.csv(dat, "Data/Tropidophis_areas.csv", row.names = FALSE)

# Read in Tropidophis records
snake_areas <- read.csv("Data/Tropidophis_areas.csv")

# Plot SAR
SARP(snake_areas, npsi = 1)
# 1 breakpoint at 18.771

# Read in tree for SpAR
# Tree from: Zaher, H., Trusz, C., Koch, C., Entiauspe-Neto, O.M., Battilana, J.,
#  & Grazziotin, F.G. (2024). Molecular phylogeny and biogeography of the dwarf
#  boas of the family Tropidophiidae (Serpentes: Alethinophidia). Systematics
#  and Biodiversity, 22(1): 2319289
snake_tree <- read.tree("Data/tropidophis_tree.tree")

# Calculate speciation rates
speciation_rates <- speciationMS2(snake_tree, label_type = "epithet", occurrences = snake_areas)

# Create SpAR
SpeARP(speciation_rates, npsi = 1)
# 1 breakpoint at 25.143
