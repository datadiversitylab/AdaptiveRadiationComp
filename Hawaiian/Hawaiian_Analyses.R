# Load libraries
library(ape)
library(SSARP)
library(checkmate) # For speciationMS2

# Load speciationMS2
source("../speciationMS2.R")

##### Pritchardia palms #####
key <- get_key("Pritchardia", "genus")
dat <- get_data(key, limit = 100000)
land <- find_land(dat)
areas <- find_areas(land)
nocont_dat <- remove_continents(areas)

# Keep only Hawaiian records
USA <- nocont_dat[which(nocont_dat$First == 'USA'),]
USA_Hawaii <- USA[which(USA$Second == 'Hawaii'),]

Hawaii <- nocont_dat[which(nocont_dat$First == 'Hawaii'),]

# Combine into one df
Hawaii_df <- rbind(USA_Hawaii, Hawaii)

# Create SAR
create_SAR(Hawaii_df)

# This might be how it actually is, but this one has a negative second slope

# Testing out list of species from Hodel, D.R., A review of the genus Pritchardia, Palms, 2007, vol. 51S
# https://web.p.ebscohost.com/ehost/detail/detail?vid=0&sid=e9bab12b-29dc-4045-a75c-0f36348742eb%40redis&bdata=JnNpdGU9ZWhvc3QtbGl2ZQ%3d%3d#db=asn&AN=28328573

setwd("C:/Users/KMart/Documents/GitHub/AdaptiveRadiationComp/Hawaiian")
palm_df <- read.csv("Data/Pritchardia_Areas.csv")
create_SAR(palm_df)
# Zero breakpoint regression with a slope of 0.203

# Create SpAR
palm_tree <- read.tree("Data/pritchardia_snatcher")
palm_sp <- speciationMS2(tree = palm_tree, label_type = "epithet", occurrences = palm_df)
# Errors out because there are no subtrees that match a single island
palm_sp <- speciationMS2(tree = palm_tree, label_type = "epithet", occurrences = Hawaii_df)
# Same thing for this one

##### Silversword alliance #####
silver_areas <- read.csv("Data/Silversword_occs.csv")

silver_SAR <- create_SAR(silver_areas)
# Breakpoint: 21.496 (convergence not attained)
confint(silver_SAR$segObj)
# Est. CI(95%).low CI(95%).up
# psi1.x 21.496     18.7533    24.2387

silver_tree <- read.tree("Data/silversword_snatcher")

# Add combined Species column
silver_areas$Species <- paste(silver_areas$Genus, silver_areas$Species, sep = "_")

silver_sp <- speciationMS2(silver_tree, label_type = "epithet", silver_areas)

create_SpAR(silver_sp)

##### Tetragnatha spiders #####
spider_areas <- read.csv("Data/Tetragnatha_Areas.csv")

spider_SAR <- create_SAR(spider_areas)
# Linear regression with a slope of 0.382

spider_tree <- read.tree("Data/Tetragnatha_snatcher")

spider_sp <- speciationMS2(spider_tree, label_type = "epithet", spider_areas)

create_SpAR(spider_sp)
# Slope of 0.0002596