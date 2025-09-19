# Load libraries
library(ape)
library(SSARP)
library(checkmate) # For speciationMS2

# Load speciationMS2
source("../speciationMS2.R")

##### Microlophus lizards #####
micro_areas <- read.csv("Data/micro_areas.csv")

# Create SAR
micro_SAR <- create_SAR(micro_areas)
# Breakpoint at 18.816
confint(micro_SAR$segObj)
# Est. CI(95%).low CI(95%).up
# psi1.x 18.8165     17.6676    19.9654

micro_tree <- read.tree("Data/micro_tree.tree")

micro_sp <- speciationMS2(micro_tree, label_type = "epithet", occurrences = micro_areas)

# Create SpAR
micro_SpAR <- create_SpAR(micro_sp, npsi = 0)
micro_SpAR

##### Scalesia Giant Daisies #####
# ExaBayes tree obtained from the c85m20 dataset was selected as optimal (In supp tree C)
scal_areas <- read.csv("Data/Scalesia_Areas.csv")

# Create SAR
scal_SAR <- create_SAR(scal_areas)
# No convergence on segmented run, breakpoint at 20.709, negative second slope
confint(scal_SAR$segObj)
# Est. CI(95%).low CI(95%).up
# psi1.x 20.7091     18.9764    22.4418

scal_tree <- read.tree("Data/Data S1C.nex")
scal_tree <- read.tree("Data/Data S1B.nex")
scal_tree <- read.tree("Data/Data S1A.nex")
scal_tree <- read.tree("Data/scalesia_snatcher")

# Grab c85m20
scal_tree <- scal_tree$`treeSVDquartets_c85m20=`
# These two have multiple individuals for the same species
scal_tree <- scal_tree$`treeExaBayes_c85m20=` 
scal_tree <- scal_tree$`treeRAxML_c85m20=`

# The tip labels are S_species so...
scal_areas$Species <- paste("S", scal_areas$Species, sep = "_")

scal_sp <- speciationMS2(scal_tree, label_type = "epithet", scal_areas)

scal_SpAR <- create_SpAR(occurrences = scal_sp, npsi = 1)
# Linear regression, slope of 0.04588

##### Geospiza #####
geo_areas <- read.csv("Data/geo_areas.csv")

# Create SAR
geo_SAR <- create_SAR(geo_areas)
# Breakpoint 19.327, flat second slope
confint(geo_SAR$segObj)
# Est. CI(95%).low CI(95%).up
# psi1.x 19.3272     17.7559    20.8984

# Geospiza trees SUCK because they're hybridizing
geo_tree <- read.tree("Data/timetree_all_taxa_OW_2019.nextree.tre")
geo_tree <- geo_tree$`treeno_root_constraint=`

# Get all the species from the bird tree
geo_sp <- c("Geospiza_difficilis_PASSERIFORMES_oscines_Thraupidae", 
            "Geospiza_fuliginosa_PASSERIFORMES_oscines_Thraupidae",
            "Geospiza_fortis_PASSERIFORMES_oscines_Thraupidae",
            "Geospiza_scandens_PASSERIFORMES_oscines_Thraupidae",
            "Geospiza_magnirostris_PASSERIFORMES_oscines_Thraupidae",
            "Geospiza_conirostris_PASSERIFORMES_oscines_Thraupidae",
            "Geospiza_septentrionalis_PASSERIFORMES_oscines_Thraupidae")

# These are the tips to drop from the tree
drop_sp <- setdiff(geo_tree$tip.label, geo_sp)

# Drop those tips from the tree
geo_tree <- ape::drop.tip(geo_tree, drop_sp)
plot(geo_tree)

geo_tree$tip.label <- c("Geospiza_septentrionalis", "Geospiza_conirostris", "Geospiza_magnirostris",
                        "Geospiza_scandens", "Geospiza_fortis", "Geospiza_fuliginosa", "Geospiza_difficilis")

geo_sp <- speciationMS2(tree = geo_tree, label_type = "epithet", occurrences = geo_areas)
# All the rates are zero, there aren't enough species in this tree

# Trying the other tree again
geo_tree <- read.tree("Data/dfinches_2018br.nwk")

# Include only one instance of each species
geo_sp <- c("Geospiza_conirostris_G",
            "Geospiza_scandens",
            "Geospiza_magnirostris",
            "Geospiza_fortis_S2",
            "Geospiza_fuliginosa_S",
            "Geospiza_difficilis_G")

# These are the tips to drop from the tree
drop_sp <- setdiff(geo_tree$tip.label, geo_sp)

# Drop those tips from the tree
geo_tree <- ape::drop.tip(geo_tree, drop_sp)
plot(geo_tree)

geo_tree$tip.label <- c("Geospiza_difficilis", "Geospiza_fuliginosa", "Geospiza_fortis", "Geospiza_magnirostris", "Geospiza_scandens", "Geospiza_conirostris")
plot(geo_tree)

geo_sp <- speciationMS2(tree = geo_tree, label_type = "epithet", occurrences = geo_areas)

geo_SpAR <- create_SpAR(geo_sp)
confint(geo_SpAR$segObj)
# Est. CI(95%).low CI(95%).up
# psi1.x 20.2801     18.2962     22.264