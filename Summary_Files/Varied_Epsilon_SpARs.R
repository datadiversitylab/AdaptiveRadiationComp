library(geiger)
library(ape)
library(ssarp)

# Source estimate_MS_AR function
source("Summary_Files/estimate_MS_AR.R")

##### Anolis #####
anole_areas <- read.csv("Caribbean/Data/Anolis_areas.csv")

# The tip labels on the tree are epithets
colnames(anole_areas)[2] <- "specificEpithet"

anole_tree <- read.tree("Caribbean/Data/Patton_Anolis_Trimmed.tree")

# Estimate diversification rates with epsilon of: 0, 0.5, and 0.9
# Then plot the SpAR
epsilon_0 <- estimate_MS_AR(tree = anole_tree, label_type = "epithet", 
                            occurrences = anole_areas, epsilon = 0)
anole_SpAR <- create_spar(epsilon_0, visualize = TRUE)
saveRDS(anole_SpAR, "Caribbean/Data/anolis_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = anole_tree, label_type = "epithet", 
                              occurrences = anole_areas, epsilon = 0.5)
anole_SpAR <- create_spar(epsilon_0.5, visualize = TRUE)
saveRDS(anole_SpAR, "Caribbean/Data/anolis_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = anole_tree, label_type = "epithet",
                              occurrences = anole_areas, epsilon = 0.9)
anole_SpAR <- create_spar(epsilon_0.9, visualize = TRUE)
saveRDS(anole_SpAR, "Caribbean/Data/anolis_spar_e09.rds")

##### Eleutherodactylus #####
frog_occs <- read.csv("Caribbean/Data/Eleutherodactylus_areas_2.csv")
# Tip labels on the tree are binomial, with an underscore separating
frog_tree <- read.tree("Caribbean/Data/frog_tree.tree")

# Estimate diversification rates with epsilon of: 0, 0.5, and 0.9
# Then plot the SpAR
epsilon_0 <- estimate_MS_AR(tree = frog_tree, label_type = "binomial", 
                            occurrences = frog_occs, epsilon = 0)
frog_SpAR <- create_spar(epsilon_0, visualize = TRUE)
saveRDS(frog_SpAR, "Caribbean/Data/frog_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = frog_tree, label_type = "binomial", 
                            occurrences = frog_occs, epsilon = 0.5)
frog_SpAR <- create_spar(epsilon_0.5, visualize = TRUE)
saveRDS(frog_SpAR, "Caribbean/Data/frog_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = frog_tree, label_type = "binomial", 
                              occurrences = frog_occs, epsilon = 0.9)
frog_SpAR <- create_spar(epsilon_0.9, visualize = TRUE)
saveRDS(frog_SpAR, "Caribbean/Data/frog_SpAR_e09.rds")

##### Finches #####
finch_occs <- read.csv("Galapagos/Data/finch_pam_areas_2.csv")

# Using the birdtree.org tree
trees <- read.nexus("Galapagos/Data/BirdTree_Finches.nex")
# Just use a single posterior tree, consensus has polytomies
# Randomly choose out of 100
sample(c(1:100), size = 1)
finch_tree <- trees[[1]] # The random number was 1

# Cactospiza_pallida is Camarhynchus pallidus
# Cactospiza_heliobates is Camarhynchus heliobates
# Fix this in the occs object
finch_occs$specificEpithet <- paste(
  finch_occs$genericName,
  finch_occs$specificEpithet,
  sep = "_"
)

# Replace old names with new names
finch_occs[which(finch_occs$specificEpithet == "Cactospiza_pallida"), 2] <- "Camarhynchus_pallidus"
finch_occs[which(finch_occs$specificEpithet == "Cactospiza_heliobates"), 2] <- "Camarhynchus_heliobates"

epsilon_0 <- estimate_MS_AR(tree = finch_tree, label_type = "epithet", 
                            occurrences = finch_occs, epsilon = 0)
finch_SpAR <- create_spar(epsilon_0, visualize = TRUE, npsi = 0)
saveRDS(finch_SpAR, "Galapagos/Data/finch_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = finch_tree, label_type = "epithet", 
                              occurrences = finch_occs, epsilon = 0.5)
finch_SpAR <- create_spar(epsilon_0.5, visualize = TRUE, npsi = 0)
saveRDS(finch_SpAR, "Galapagos/Data/finch_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = finch_tree, label_type = "epithet", 
                              occurrences = finch_occs, epsilon = 0.9)
finch_SpAR <- create_spar(epsilon_0.9, visualize = TRUE, npsi = 0)
saveRDS(finch_SpAR, "Galapagos/Data/finch_SpAR_e09.rds")

##### Scalesia #####
scal_occs <- read.csv("Galapagos/Data/Scalesia_Areas.csv")
scal_tree <- read.tree("Galapagos/Data/scalesia_snatcher")

# Remove periods from species name in occs since they're not in the tips
scal_occs$SpeciesName <- gsub("\\. ", "_", scal_occs$SpeciesName)

# Change column name
colnames(scal_occs)[1] <- "specificEpithet"

epsilon_0 <- estimate_MS_AR(tree = scal_tree, label_type = "epithet", 
                            occurrences = scal_occs, epsilon = 0)
scal_SpAR <- create_spar(epsilon_0, visualize = TRUE, npsi = 1)
saveRDS(scal_SpAR, "Galapagos/Data/scalesia_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = scal_tree, label_type = "epithet", 
                            occurrences = scal_occs, epsilon = 0.5)
scal_SpAR <- create_spar(epsilon_0.5, visualize = TRUE, npsi = 1)
saveRDS(scal_SpAR, "Galapagos/Data/scalesia_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = scal_tree, label_type = "epithet", 
                              occurrences = scal_occs, epsilon = 0.9)
scal_SpAR <- create_spar(epsilon_0.9, visualize = TRUE, npsi = 1)
saveRDS(scal_SpAR, "Galapagos/Data/scalesia_SpAR_e09.rds")

##### Silverswords #####
silver_occs <- read.csv("Hawaiian/Data/Silversword_Areas.csv")
# Make sure that the Species column is called "specificEpithet"
colnames(silver_occs)[2] <- "specificEpithet"

silver_tree <- read.tree("Hawaiian/Data/silversword_snatcher")

epsilon_0 <- estimate_MS_AR(tree = silver_tree, label_type = "binomial", 
                            occurrences = silver_occs, epsilon = 0)
silver_SpAR <- create_spar(epsilon_0, visualize = TRUE, npsi = 1)
saveRDS(silver_SpAR, "Hawaiian/Data/silver_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = silver_tree, label_type = "binomial", 
                            occurrences = silver_occs, epsilon = 0.5)
silver_SpAR <- create_spar(epsilon_0.5, visualize = TRUE, npsi = 1)
saveRDS(silver_SpAR, "Hawaiian/Data/silver_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = silver_tree, label_type = "binomial", 
                              occurrences = silver_occs, epsilon = 0.9)
silver_SpAR <- create_spar(silver_epsilon09, visualize = TRUE, npsi = 1)
saveRDS(silver_SpAR, "Hawaiian/Data/silver_SpAR_e09.rds")

##### Tetragnatha #####
spider_occs <- read.csv("Hawaiian/Data/tetragnatha_filtered_occs.csv")
spider_tree <- read.tree("Hawaiian/Data/Tetragnatha_snatcher")

epsilon_0 <- estimate_MS_AR(tree = spider_tree, label_type = "epithet", 
                            occurrences = spider_occs, epsilon = 0)
spider_SpAR <- create_spar(epsilon_0, visualize = TRUE, npsi = 0)
saveRDS(spider_SpAR, "Hawaiian/Data/spider_SpAR_e0.rds")

epsilon_0.5 <- estimate_MS_AR(tree = spider_tree, label_type = "epithet", 
                            occurrences = spider_occs, epsilon = 0.5)
spider_SpAR <- create_spar(epsilon_0.5, visualize = TRUE, npsi = 0)
saveRDS(spider_SpAR, "Hawaiian/Data/tetragnatha_SpAR_e05.rds")

epsilon_0.9 <- estimate_MS_AR(tree = spider_tree, label_type = "epithet", 
                              occurrences = spider_occs, epsilon = 0.9)
spider_SpAR <- create_spar(epsilon_0.9, visualize = TRUE, npsi = 0)
saveRDS(spider_SpAR, "Hawaiian/Data/spider_SpAR_e09.rds")
