##### Archipelago x Lineage GLMM With Model Selection #####
library(lme4)
library(MuMIn)
library(dplyr)

# Read dataset
dat <- read.csv("Summary_Files/AxL.csv")

# Scale the columns that should be scaled
dat_scale <- scale(dat[,5:20])
dat_scale <- cbind(dat$lineage, dat$archipelago, dat$bp, dat$dispersal, dat_scale)
dat_scale <- as.data.frame(dat_scale)
colnames(dat_scale)[1] <- "lineage"
colnames(dat_scale)[2] <- "archipelago"
colnames(dat_scale)[3] <- "bp"
colnames(dat_scale)[4] <- "dispersal"

# They're all characters for some reason
dat_scale[,5:20] <- sapply(dat_scale[,5:20], as.numeric)
dat_scale$bp <- as.numeric(dat_scale$bp)

# Make lineage, archipelago, and dispersal factors
dat_scale$lineage <- as.factor(dat_scale$lineage)
dat_scale$archipelago <- as.factor(dat_scale$archipelago)
dat_scale$dispersal <- as.factor(dat_scale$dispersal)

##### Create correlation figure #####
library(corrplot)
cor_matrix_scale <- cor(dat_scale[, colnames(dat_scale)[5:20]])
corrplot(cor_matrix_scale, method = "number", order = "alphabet", 
         type = "lower", col = COL2("RdBu"))

# Figure out which variables are highly correlated with which
# Convert correlation matrix to long format
cor_df <- as.data.frame(as.table(cor_matrix_scale))

# Rename columns
names(cor_df) <- c("var1", "var2", "correlation")

# Remove self-correlations
cor_df <- cor_df[cor_df$var1 != cor_df$var2, ]

# Round correlations to 2 decimal places (mostly so the 0.697 ones are 0.7)
cor_df$correlation <- round(cor_df$correlation, 2)

# Include only |r| >= 0.7
cor_df <- cor_df[abs(cor_df$correlation) >= 0.7, ]

# Remove duplicated pairs (e.g., var1 = A | var 2 = B and var 1 = B | var 2 = A)
pair_id <- apply(cor_df[, c("var1", "var2")], 1, function(x) {
  paste(sort(x), collapse = "_")
})

cor_df <- cor_df[!duplicated(pair_id), ]

# OKAY there are so many big r values
# n_islands: mean_elevation, mean_TRI, max_TRI, mean_nearest_dist, min_nearest_dist, max_area, max_richness
# mean_csi: max_elevation, min_area, mean_area
# mean_elevation: mean_TRI, max_TRI, mean_nearest_dist, min_nearest_dist, min_area, mean_area
# max_elevation: max_nearest_dist
# mean_TRI: max_TRI, mean_nearest_dist, min_nearest_dist, min_area, max_area, mean_area, min_richness_nonzero
# max_TRI: mean_nearest_dist, min_nearest_dist, max_area, max_richness, n_habitat
# mean_nearest_dist: min_nearest_dist, min_area, mean_area
# min_nearest_dist: min_area, max_area, mean_area
# min_area: mean_area
# max_area: max_richness
# min_richness_nonzero: mean_richness_nonzero
# max_richness: n_habitat

# KEEP?: n_islands, mean_csi, mean_nearest_dist, mean_elevation, mean_TRI, 
#        max_richness, n_habitat

cor_matrix_new <- cor(dat_scale[, c("n_islands", "mean_csi", "mean_nearest_dist", 
                                    "mean_elevation", "mean_TRI", "max_richness",
                                    "n_habitat")])
# Everything is still ridiculously correlated I give up

##### All Subsets Model Creation #####
# THIS IS A HORRIBLE MESS OF SINGULARITY
# Start with one variable, then systematically add the rest to the model
var_vec <- c("n_islands", "mean_csi", "mean_nearest_dist", 
             "mean_elevation", "mean_TRI", "max_richness",
             "n_habitat")

# For counting loop iterations
i <- 0

# Create a list to store model objects
model_list <- list()

# The combn function generates all combinations of the elements of x
#  taken m at a time. I can use this function if I iterate through
#  var_vec using indices instead of names

for(v in c(1:length(var_vec))){
  # Gets all combinations up to the vth position
  # simplify = FALSE so it returns a list
  combos <- combn(var_vec, v, simplify = FALSE)
  
  for(var in combos){
    # Write out current variable list separated by +'s for formula
    current_vars <- paste(var, collapse = "+")
    
    # Next, write the whole formula
    formula <- paste("bp ~", current_vars, "+ (1|archipelago)")
    
    # Use the formula in a GLMM
    model <- glmer(formula = formula, data = dat_scale, family = binomial)
    
    # Add model object to the list
    i <- i + 1
    model_list[[i]] <- model
    
  }
}

# Now use model.sel to select the best-supported model based on AICc
selection <- model.sel(model_list)
