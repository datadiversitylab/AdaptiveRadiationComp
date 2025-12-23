##### Island x Lineage GLMM With Model Selection #####
library(lme4)
library(MuMIn)
library(dplyr)

# Read dataset
dat <- read.csv("Summary_Files/IxL.csv")

# Scale the columns that should be scaled
dat_scale <- scale(dat[,2:13])
dat_scale <- cbind(dat$name, dat_scale, dat$lineage, dat$dispersal, dat$n_habitat, dat$archipelago)
dat_scale <- as.data.frame(dat_scale)
colnames(dat_scale)[1] <- "name"
colnames(dat_scale)[14] <- "lineage"
colnames(dat_scale)[15] <- "dispersal"
colnames(dat_scale)[16] <- "n_habitat"
colnames(dat_scale)[17] <- "archipelago"

# They're all characters for some reason
dat_scale[,2:13] <- sapply(dat_scale[,2:13], as.numeric)
dat_scale$n_habitat <- as.numeric(dat_scale$n_habitat)

# Make sure that richness and presence are not scaled
dat_scale$richness <- dat$richness
dat_scale$presence <- dat$presence

# Make dispersal, archipelago, and presence factors
dat_scale$dispersal <- as.factor(dat_scale$dispersal)
dat_scale$archipelago <- as.factor(dat_scale$archipelago)
dat_scale$presence <- as.factor(dat_scale$presence)

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
                                      "n_habitat")])
corrplot(cor_matrix_scale, method = "number", order = "alphabet", 
         type = "lower", col = COL2("RdBu"))

##### Forward Stepwise Model Creation #####

# Start with one variable, then systematically add the rest to the model
var_vec <- c("TRI", "max_elev", "Nearest_Dist", 
             "mean_csi", "sd_csi", "dispersal",
             "n_habitat", "area")

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
# This is model 186 from the list
best <- model_list[[186]]
# Formula: presence ~ TRI + Nearest_Dist + mean_csi + dispersal + n_habitat + (1 | archipelago)

R2 <- r.squaredGLMM(best)
#               R2m       R2c
# theoretical 0.2883364 0.7901236
# delta       0.2720779 0.7455708

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

# Calculate variable importance (sum of model weights)
var_import <- sw(deltaAIC4)
#                     n_habitat Nearest_Dist mean_csi TRI  dispersal sd_csi max_elev area
# Sum of weights:      1.00      1.00         0.94    0.85  0.72      0.34   0.23     0.15

# Barplot
import_vals <- c(1, 1, 0.94, 0.85, 0.72, 0.34, 0.23, 0.15)
barplot(import_vals, names.arg = names(var_import),
        xlab = "Predictors", ylab = "Variable Importance", main = "delta AIC < 4 subset")



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
        