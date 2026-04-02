##### Island x Lineage GLMM With Model Selection #####
library(lme4)
library(MuMIn)
library(dplyr)
library(scales)

# Read dataset
dat <- read.csv("Summary_Files/IxL_distocc_2_17.csv")

# If an island has an NA for dist_occ_islands,
#  fill in the value from nearest_occ
for(i in c(1:length(dat$name))){
  if(is.na(dat$dist_occ_islands[i])){
    dat$dist_occ_islands[i] <- dat$nearest_occ[i]
  }
}

# CARIBBEAN ONLY
dat <- dat[which(dat$archipelago == "carib"),]

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
# dat_scale$dispersal <- ifelse(dat_scale$dispersal == 2, yes = 1, no = 0)

# Make dispersal, archipelago, and presence factors
dat_scale$dispersal <- as.factor(dat_scale$dispersal)
dat_scale$archipelago <- as.factor(dat_scale$archipelago)
dat_scale$presence <- as.factor(dat_scale$presence)

# Make sure that you're only using islands where the richness is > 0
dat_scale <- dat_scale[which(dat_scale$richness > 0),]

# Actually, scale richness to be between 0 and 1
dat_scale$richness <- rescale(dat_scale$richness, to = c(0,1))

# Global model
global_model <- glmer(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago), 
                      data = dat_scale, 
                      family = binomial) # (link = "logit")

# Doesn't converge, so restart from a previous fit, with more iterations
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
ss <- getME(global_model, c("theta","fixef"))
global_model <- update(global_model, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(global_model)
# Significant: Nearest_Dist, mean_csi, n_habitat

##### Create correlation figure #####
library(corrplot)
cor_matrix_scale <- cor(dat_scale[, c("TRI", "mean_elev", "median_elev", 
                                      "min_elev", "max_elev", "Nearest_Dist", 
                                      "mean_csi", "sd_csi", "area",
                                      "n_habitat", "dist_mainland", "dist_occ_islands")])
corrplot(cor_matrix_scale, method = "number", order = "alphabet", 
         type = "lower", col = COL2("RdBu"))

##### Forward Stepwise Model Creation #####

# Start with one variable, then systematically add the rest to the model
# var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
#              "mean_csi", "sd_csi", "dispersal",
#              "n_habitat", "area", "dist_mainland",
#              "dist_occ_islands")

var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi",
             "n_habitat", "area", "dist_mainland",
             "dist_occ_islands")

# Actually, dist_occ_islands can't be included for the presence models
#  because islands without a presence weren't included in their caclulation

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
    #formula <- paste("presence ~", current_vars, "+ (1|archipelago)")
    # Caribbean only
    formula <- paste("presence ~", current_vars, "+ (1|lineage)")
    
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
# Formula: presence ~ Nearest_Dist + dispersal + n_habitat + dist_mainland + (1 | archipelago)
# Formula: presence ~ TRI + Nearest_Dist + n_habitat + dist_mainland + (1 | archipelago)

R2 <- r.squaredGLMM(best)
#             R2m       R2c
# theoretical 0.5879026 0.7326733
# delta       0.5463199 0.6808510

# Once they found the best-fit model, Cardoso et al. used dredge to calculate 
#  model-averaged coefficients and variable importance
# Since I already have all of the models that include these variables, I can
#  just grab them from the model list, right?

# All models that include TRI
TRI_mod <- which(!is.na(selection$TRI))

# All models that include Nearest_Dist
ND_mod <- which(!is.na(selection$Nearest_Dist))

# All models that include mean_csi
csi_mod <- which(!is.na(selection$mean_csi))

# All models that include dispersal
dis_mod <- which(!is.na(selection$dispersal))

# All models that include n_habitat
hab_mod <- which(!is.na(selection$n_habitat))

# A lot of these are shared, so create a vector of unique numbers
all_mod <- c(TRI_mod, ND_mod, csi_mod, dis_mod, hab_mod)
uniq_mod <- unique(all_mod)

# All of the info about these models is here:
best_2 <- selection[uniq_mod,]
# The model numbers that can be used to index model_list are:
inc_models <- rownames(best_2)
inc_models <- as.numeric(inc_models)

# Now subset all_models list
avg_models <- model_list[inc_models]

# Calculate model-averaged coefficients
avg_coeff <- model.avg(avg_models)
summary(avg_coeff)

coef(avg_coeff)

# Calculate coefficient confidence intervals
conf_int <- confint(avg_coeff)

# Calculate variable importance (sum of model weights)
var_import <- sw(avg_models)
#                     n_habitat Nearest_Dist mean_csi TRI  dispersal sd_csi max_elev area
# Sum of weights:      1.00      0.99         0.84     0.75 0.63      0.39   0.34     0.27

# Barplot
import_vals <- c(1, 0.99, 0.84, 0.75, 0.63, 0.39, 0.34, 0.27)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance")


##### Compare the above results to models with deltaAIC < 4 #####

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


##### Richness as Response #####


# Scale richness to be in (0,1) so the beta distribution works
# https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
dat_scale$richness <- (dat_scale$richness * (nrow(dat_scale) - 1) + 0.5) / nrow(dat_scale)

# Start with one variable, then systematically add the rest to the model
# var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
#              "mean_csi", "sd_csi", "dispersal",
#              "n_habitat", "area", "dist_mainland")

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
    #formula <- paste("richness ~", current_vars, "+ (1|lineage)")
    
    # Have to convert to proper formula?
    formula <- formula(formula)
    
    # Use the formula in a GLMM
    model <- glmer(formula = formula, data = dat_scale, family = poisson)
    # Now that the response is between 0 and 1, use beta distribution
    #model <- glmmTMB(formula = formula, data = dat_scale, family = beta_family)
    
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
# Formula: richness ~ TRI + max_elev + dispersal + n_habitat + area + (1 | archipelago)
# Beta formula:  richness ~ max_elev + dispersal + n_habitat + area + (1 | archipelago)
# richness ~ TRI + max_elev + dispersal + n_habitat + area + (1 | archipelago)
# richness ~ TRI + n_habitat + area + dist_occ_islands + (1 | archipelago)

R2 <- r.squaredGLMM(best)
#                 R2m       R2c
# delta     0.5975533 0.8339208
# lognormal 0.6046249 0.8437896
# trigamma  0.5895613 0.8227674

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
# BETA: max_elev, n_habitat, area
# New dispersal: TRI, -dispersal1, n_habitat, area, dist_mainland
# No dispersal, added dist_occ_islands: TRI, n_habitat, area, dist_occ_islands

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
# area, dist_occ_islands, n_habitat

# Beta
import_vals <- c(1, 1, 0.85, 0.77, 0.53, 0.35, 0.25, 0.22, 0.16)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance", 
        main = "delta AIC < 4 subset",
        cex.names = 0.7)
# area, n_habitat

##### Richness as Response WITH ALL SCALE #####
# Read dataset
dat <- read.csv("Summary_Files/IxL_distmainland.csv")

# Scale appropriate columns to be between 0 and 1
dat_scale <- dat
dat_scale$TRI <- rescale(dat_scale$TRI, to = c(0,1))
dat_scale$max_elev <- rescale(dat_scale$max_elev, to = c(0,1))
dat_scale$Nearest_Dist <- rescale(dat_scale$Nearest_Dist, to = c(0,1))
dat_scale$mean_csi <- rescale(dat_scale$mean_csi, to = c(0,1))
dat_scale$sd_csi <- rescale(dat_scale$sd_csi, to = c(0,1))

# Turn dispersal into 0,1,2 so the reference is Low
dat_scale$dispersal[which(dat_scale$dispersal=="low")] <- 0
dat_scale$dispersal[which(dat_scale$dispersal=="moderate")] <- 1
dat_scale$dispersal[which(dat_scale$dispersal=="high")] <- 2
# Make sure that dispersal is a factor
dat_scale$dispersal <- as.factor(dat_scale$dispersal)

dat_scale$n_habitat <- rescale(dat_scale$n_habitat, to = c(0,1))
dat_scale$area <- rescale(dat_scale$area, to = c(0,1))
dat_scale$dist_mainland <- rescale(dat_scale$dist_mainland, to = c(0,1))

# Richness
dat_scale$richness <- rescale(dat_scale$richness, to = c(0,1))
# Scale richness to be in (0,1) so the beta distribution works
# https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
dat_scale$richness <- (dat_scale$richness * (nrow(dat_scale) - 1) + 0.5) / nrow(dat_scale)

# Start with one variable, then systematically add the rest to the model
var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi", "dispersal",
             "n_habitat", "area", "dist_mainland")

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
    #model <- glmer(formula = formula, data = dat_scale, family = poisson)
    # Now that the response is between 0 and 1, use beta distribution
    model <- glmmTMB(formula = formula, data = dat_scale, family = beta_family)
    
    # Add model object to the list
    i <- i + 1
    model_list[[i]] <- model
    
  }
}

# Now use model.sel to select the best-supported model based on AICc
selection <- model.sel(model_list)

# The best model has a delta of 0 and is in the first row
# This is model 510 from the list
best <- model_list[[510]]
# Formula: richness ~ max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + dist_mainland + (1 | archipelago)

R2 <- r.squaredGLMM(best)
# R2m       R2c
# 0.547727 0.7567428

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
# max_elev, nearest_dist, mean_csi, sd_csi, dispersal1, n_habitat, area, dist_mainland

# Calculate variable importance (sum of model weights)
var_import <- sw(deltaAIC4)
# cond(area) cond(dispersal)  cond(max_elev)   cond(n_habitat)  cond(Nearest_Dist)
#       1.00            1.00            1.00              1.00                1.00

# Barplot
import_vals <- c(1, 1, 1, 1, 1, 0.94, 0.93, 0.90, 0.30)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance", 
        main = "delta AIC < 4 subset",
        cex.names = 0.7)


##### OLD CODE but might be useful later #####

options(na.action = "na.fail") # Required for dredge to run
all_subsets <- dredge(global_model, rank = "BIC")
print(all_subsets)

# Start with one variable, then systematically add the rest to the model
var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi", "dispersal",
             "n_habitat", "area")
# Assign names to the vector elements so I can index them by name
names(var_vec) <- var_vec

# For counting loop iterations
i <- 0

# Create a list to store model objects
model_list <- list()
# Actually this logic won't work
for(var in var_vec){
  # Run model with just the current variable
  formula <- paste("presence ~", var, "+ (1|archipelago)")
  # model <- glmer(formula = formula,
  #                data = dat_scale,
  #                family = binomial)
  rest_var <- var_vec[setdiff(var_vec, var)]
  i <- i + 1
  model_list[[i]] <- formula
  for(v in rest_var){
    formula <- paste("presence ~", var, "+", v, "+ (1|archipelago)")
    i <- i + 1
    model_list[[i]] <- formula
  }
  
}
####
        