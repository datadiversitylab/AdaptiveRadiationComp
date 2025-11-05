library(here)

# Read the data
data <- read.csv(here("Summary_Files", "AxL.csv"), stringsAsFactors = FALSE)
data <- data[, -1]

# Convert categorical variables to factors
data$archipelago <- as.factor(data$archipelago)
data$lineage <- as.factor(data$lineage)
data$dispersal <- as.factor(data$dispersal)
data$diet <- as.factor(data$diet)

# Identify and scale numeric predictors
exclude_vars <- c("bp", "lineage", "archipelago")
all_vars <- names(data)
numeric_predictors <- all_vars[!all_vars %in% exclude_vars & 
                                 sapply(data, is.numeric)]

data_scaled <- data
for (var in numeric_predictors) {
  if (sd(data[[var]], na.rm = TRUE) > 0) {
    data_scaled[[var]] <- scale(data[[var]])
  }
}


# Random intercept model

model_full <- glmer(
  bp ~ n_islands + mean_csi + mean_elevation +
    mean_TRI + mean_nearest_dist + mean_area + 
    dispersal + n_habitat + diet + (1|archipelago),
  data = data_scaled,
  family = binomial(link = "logit"))

summary(model_full)

# Stepwise model selection (archipelago as random effect)

univariate_results <- data.frame(
  predictor = character(),
  estimate = numeric(),
  std_error = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

for (var in numeric_predictors) {
  if (sd(data_scaled[[var]], na.rm = TRUE) == 0) next
  
  formula_str <- paste("bp ~", var, "+ (1|archipelago)")
  
  tryCatch({
    model_uni <- glmer(
      as.formula(formula_str),
      data = data_scaled,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa",
                             calc.derivs = FALSE,
                             check.conv.singular = "ignore")
    )
    
    coef_summary <- summary(model_uni)$coefficients
    if (nrow(coef_summary) > 1) {  # Check if predictor coefficient exists
      univariate_results <- rbind(univariate_results, data.frame(
        predictor = var,
        estimate = coef_summary[2, 1],
        std_error = coef_summary[2, 2],
        z_value = coef_summary[2, 3],
        p_value = coef_summary[2, 4],
        AIC = AIC(model_uni),
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    cat("Could not fit model for", var, "\n")
  })
}

for (var in c("dispersal", "diet")) {
  formula_str <- paste("bp ~", var, "+ (1|archipelago)")
  
  tryCatch({
    model_uni <- glmer(
      as.formula(formula_str),
      data = data_scaled,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa",
                             calc.derivs = FALSE,
                             check.conv.singular = "ignore")
    )
    
    coef_summary <- summary(model_uni)$coefficients
    if (nrow(coef_summary) > 1) {
      # For categorical, take the first non-intercept coefficient
      univariate_results <- rbind(univariate_results, data.frame(
        predictor = var,
        estimate = coef_summary[2, 1],
        std_error = coef_summary[2, 2],
        z_value = coef_summary[2, 3],
        p_value = coef_summary[2, 4],
        AIC = AIC(model_uni),
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    cat("Could not fit model for", var, "\n")
  })
}

univariate_results <- univariate_results[order(univariate_results$AIC), ]

# Fixed effects only model

# Calculate importance score (combination of effect size and significance)
# Using |z-value| as primary measure

importance_df <- data.frame(
  predictor = univariate_results$predictor,
  abs_z_value = abs(univariate_results$z_value),
  abs_estimate = abs(univariate_results$estimate),
  p_value = univariate_results$p_value,
  AIC = univariate_results$AIC,
  stringsAsFactors = FALSE
)
importance_df$importance_score <- abs(importance_df$abs_z_value) * 
  (1 - pmin(importance_df$p_value, 1))
importance_df <- importance_df[order(-importance_df$importance_score), ]

importance_df

# Model comparison with different predictor combinations

model_comparison <- data.frame(
  model = character(),
  AIC = numeric(),
  BIC = numeric(),
  deviance = numeric(),
  df = numeric(),
  stringsAsFactors = FALSE
)

# Null model (archipelago only)
model_null <- glm(bp ~ archipelago, data = data_scaled, 
                  family = binomial(link = "logit"))
model_comparison <- rbind(model_comparison, data.frame(
  model = "Null (archipelago only)",
  AIC = AIC(model_null),
  BIC = BIC(model_null),
  deviance = deviance(model_null),
  df = model_null$df.residual,
  stringsAsFactors = FALSE
))

# Add models with top 1, 2, 3 predictors
for (i in 1:min(3, nrow(importance_df))) {
  predictors_subset <- importance_df$predictor[1:i]
  formula_str <- paste("bp ~ archipelago +", 
                       paste(predictors_subset, collapse = " + "))
  
  tryCatch({
    model_temp <- glm(as.formula(formula_str), 
                      data = data_scaled, 
                      family = binomial(link = "logit"))
    
    model_comparison <- rbind(model_comparison, data.frame(
      model = paste("Top", i, "predictor(s):", 
                    paste(predictors_subset, collapse = ", ")),
      AIC = AIC(model_temp),
      BIC = BIC(model_temp),
      deviance = deviance(model_temp),
      df = model_temp$df.residual,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    cat("Could not fit model with", i, "predictors\n")
  })
}

model_comparison

# Best model
best_model_idx <- which.min(model_comparison$AIC)
model_comparison[best_model_idx,]
