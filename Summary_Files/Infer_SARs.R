# Inferring SARs for the taxa of interest
# NOTE: Assumes that the working directory is the root of the GitHub repo

# Load libraries
library(ssarp)

##### Caribbean Anolis #####
# Read in richness data
anole_dat <- read.csv("Caribbean/Data/anolis_occs.csv")

# Infer SAR
anole_SAR <- create_SAR(anole_dat, npsi = 1, visualize = TRUE)
confint(anole_SAR$segObj)
#        Est.        CI(95%).low CI(95%).up
# psi1.x 22.1565     21.3179     22.995

# Write file
saveRDS(anole_SAR, "Caribbean/Data/anole_SAR.rds")

##### Caribbean Eleutherodactylus #####
# Read in richness data
frog_dat <- read.csv("Caribbean/Data/Eleutherodactylus_areas_2.csv")

# Infer SAR
frog_SAR <- create_SAR(frog_dat, npsi = 1, visualize = TRUE)
confint(frog_SAR$segObj)
#        Est.        CI(95%).low CI(95%).up
# psi1.x 21.1851     19.7247     22.6454

# Write file
saveRDS(frog_SAR, "Caribbean/Data/frog_SAR.rds")

##### Galapagos finches #####
# Read in richness data
finch_dat <- read.csv("Galapagos/Data/finch_pam_areas_2.csv")

# Infer SAR
finch_SAR <- create_SAR(finch_dat, npsi = 1, visualize = TRUE)

# Write file
saveRDS(finch_SAR, "Galapagos/Data/finch_SAR.rds")

##### Galapagos Scalesia #####
# Read in richness data
scal_dat <- read.csv("Galapagos/Data/Scalesia_Areas.csv")
# Ensure that species column name matches requirement for create_SAR
colnames(scal_dat)[3] <- "specificEpithet"

# Infer SAR
scal_SAR <- create_SAR(scal_dat, npsi = 1, visualize = TRUE)
confint(scal_SAR$segObj)
#        Est.        CI(95%).low CI(95%).up
# psi1.x 20.7091     18.9764     22.4418

# Write file
saveRDS(scal_SAR, "Galapagos/Data/scal_SAR.rds")


##### Hawaiian Tetragnatha #####
# Read in richness data
spider_dat <- read.csv("Hawaiian/Data/Tetragnatha_Areas.csv")
# Ensure that species column name matches requirement for create_SAR
colnames(spider_dat)[3] <- "specificEpithet"

# Infer SAR
spider_SAR <- create_SAR(spider_dat, npsi = 1, visualize = TRUE)

# Write file
saveRDS(spider_SAR, "Hawaiian/Data/spider_SAR.rds")

##### Hawaiian Silverswords #####
# Read in richness data
silver_dat <- read.csv("Hawaiian/Data/Silversword_Areas.csv")
# Ensure that species column name matches requirement for create_SAR
colnames(silver_dat)[2] <- "specificEpithet"

# Infer SAR
silver_SAR <- create_SAR(silver_dat, npsi = 1, visualize = TRUE)
confint(silver_SAR$segObj)
#        Est.        CI(95%).low CI(95%).up
# psi1.x 21.3544     18.5962     24.1127

# Write file
saveRDS(silver_SAR, "Hawaiian/Data/silver_SAR.rds")