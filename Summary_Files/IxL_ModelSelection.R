##### Island x Lineage GLMM With Model Selection #####
library(lme4)
library(MuMIn)
library(dplyr)

# Read dataset (assumes working directory is the root of the repo)
dat <- read.csv("Summary_Files/IxL.csv")

# If an island has an NA for dist_occ_islands,
#  fill in the value from nearest_occ
for(i in c(1:length(dat$name))){
  if(is.na(dat$dist_occ_islands[i])){
    dat$dist_occ_islands[i] <- dat$nearest_occ[i]
  }
}

# Scale the columns that should be scaled
dat_scale <- scale(dat[,2:15])
dat_scale <- cbind(dat$name, dat_scale, dat$lineage, dat$dispersal, dat$n_habitat, dat$archipelago)
dat_scale <- as.data.frame(dat_scale)
colnames(dat_scale)[1] <- "name"
colnames(dat_scale)[16] <- "lineage"
colnames(dat_scale)[17] <- "dispersal"
colnames(dat_scale)[18] <- "n_habitat"
colnames(dat_scale)[19] <- "archipelago"

# They're all characters for some reason
dat_scale[,2:15] <- sapply(dat_scale[,2:15], as.numeric)
dat_scale$n_habitat <- as.numeric(dat_scale$n_habitat)

# Make sure that richness and presence are not scaled
dat_scale$richness <- dat$richness
dat_scale$presence <- dat$presence

# Turn dispersal into 0,1,2 so the reference is Low
dat_scale$dispersal[which(dat_scale$dispersal=="low")] <- 0
dat_scale$dispersal[which(dat_scale$dispersal=="moderate")] <- 1
dat_scale$dispersal[which(dat_scale$dispersal=="high")] <- 2

# Make dispersal, archipelago, and presence factors
dat_scale$dispersal <- as.factor(dat_scale$dispersal)
dat_scale$archipelago <- as.factor(dat_scale$archipelago)
dat_scale$presence <- as.factor(dat_scale$presence)

##### Create correlation figure #####
library(corrplot)
cor_matrix_scale <- cor(dat_scale[, c("TRI", "mean_elev", "median_elev", 
                                      "min_elev", "max_elev", "Nearest_Dist", 
                                      "mean_csi", "sd_csi", "area",
                                      "n_habitat", "dist_mainland", "dist_occ_islands")])
corrplot(cor_matrix_scale, method = "number", order = "alphabet", 
         type = "lower", col = COL2("RdBu"))

##### Information Theoretic Model Selection: Presence as Response #####

# Start with one variable, then systematically add the rest to the model

var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi",
             "n_habitat", "area", "dist_mainland",
             "dist_occ_islands")

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
    formula <- paste("presence ~", current_vars, "+ (1|archipelago)")
    
    # Use the formula in a GLMM
    model <- glmer(formula = formula, data = dat_scale, family = binomial)
    
    # Add model object to the list
    i <- i + 1
    model_list[[i]] <- model
    
  }
}

# Now use model.sel to select the best-supported model based on AICc
selection <- model.sel(model_list)

# The best model has a delta of 0 and is in the first row
# This is model 369 from the list
best <- model_list[[369]]
# Formula: presence ~ Nearest_Dist + mean_csi + n_habitat + dist_mainland + 
#                     dist_occ_islands + (1 | archipelago)

R2 <- r.squaredGLMM(best)
#             R2m       R2c
# theoretical 0.5918749 0.7564765
# delta       0.5534972 0.7074259

##### Presence: Models with deltaAIC < 4 #####

# Find which models have a deltaAIC < 4
close_models <- which(selection$delta < 4)
close_models <- selection[close_models,]

# The indices of these models are the rownames
AIC4_models <- rownames(close_models)
AIC4_models <- as.numeric(AIC4_models)

# Now subset all_models list
deltaAIC4 <- model_list[AIC4_models]

# Calculate model-averaged coefficients
avg_coeff_4 <- model.avg(deltaAIC4)
summary(avg_coeff_4)

coef(avg_coeff_4)

# Calculate coefficient confidence intervals
conf_int <- confint(avg_coeff_4)
# Nearest_Dist, n_habitat, dist_mainland

# Calculate variable importance (sum of model weights)
var_import <- sw(deltaAIC4)
#                      dist_mainland dist_occ_islands n_habitat Nearest_Dist mean_csi area TRI  max_elev sd_csi
# Sum of weights:      1.00          1.00             1.00      1.00         0.59     0.30 0.23 0.22     0.20 

# Barplot
import_vals <- c(1, 1, 1, 1, 0.59, 0.30, 0.23, 0.22, 0.20)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance", 
        main = "delta AIC < 4 subset",
        cex.names = 0.7)


##### Information Theoretic Model Selection: Richness as Response #####

# Make sure that you're only using islands where the richness is > 0
dat_scale <- dat_scale[which(dat_scale$richness > 0),]

# Start with one variable, then systematically add the rest to the model

var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi",
             "n_habitat", "area", "dist_mainland",
             "dist_occ_islands")

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
    formula <- paste("richness ~", current_vars, "+ (1|archipelago)")
    
    # Have to convert to proper formula?
    formula <- formula(formula)
    
    # Use the formula in a GLMM
    model <- glmer(formula = formula, data = dat_scale, family = poisson)
    
    # Add model object to the list
    i <- i + 1
    model_list[[i]] <- model
    
  }
}

# Now use model.sel to select the best-supported model based on AICc
selection <- model.sel(model_list)

# The best model has a delta of 0 and is in the first row
# This is model 183 from the list
best <- model_list[[183]]
# Formula: richness ~ TRI + n_habitat + area + dist_occ_islands + (1 | archipelago)

R2 <- r.squaredGLMM(best)
#           R2m       R2c
# delta     0.5275299 0.8622588
# lognormal 0.5326972 0.8707048
# trigamma  0.5216649 0.8526723

##### Richness: Models with deltaAIC < 4 #####
close_models <- which(selection$delta < 4)
close_models <- selection[close_models,]

# The indices of these models are the rownames
AIC4_models <- rownames(close_models)
AIC4_models <- as.numeric(AIC4_models)

# Now subset all_models list
deltaAIC4 <- model_list[AIC4_models]

# Calculate model-averaged coefficients
avg_coeff_4 <- model.avg(deltaAIC4)
summary(avg_coeff_4)

coef(avg_coeff_4)

# Calculate coefficient confidence intervals
conf_int <- confint(avg_coeff_4)

# Calculate variable importance (sum of model weights)
var_import <- sw(deltaAIC4)
#                      area dist_occ_islands n_habitat TRI  dist_mainland max_elev Nearest_Dist sd_csi mean_csi
# Sum of weights:      1.00 1.00             1.00      0.98 0.47          0.43     0.36         0.24   0.18  

# Barplot
import_vals <- c(1, 1, 1, 0.98, 0.47, 0.43, 0.36, 0.24, 0.18)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance", 
        main = "delta AIC < 4 subset",
        cex.names = 0.7)
