# Load libraries
library(segmented)

# Load SAR objects
# Caribbean
anole_SAR <- readRDS("Caribbean/Data/anole_SAR.rds")
frog_SAR <- readRDS("Caribbean/Data/frog_SAR.rds")
# Galapagos
scal_SAR <- readRDS("Galapagos/Data/scal_SAR.rds")
finch_SAR <- readRDS("Galapagos/Data/finch_SAR.rds")
# Hawaii
silver_SAR <- readRDS("Hawaiian/Data/silver_SAR.rds")
spider_SAR <- readRDS("Hawaiian/Data/spider_SAR.rds")

set.seed(42)
n_sim <- 1000

#mean breakpoint in the Caribbean
caribbean_breakpoint_anolis <- 22.2
caribbean_breakpoint_eleuth <- 21.2
caribbean_breakpoint_mean   <- mean(c(caribbean_breakpoint_anolis,
                                      caribbean_breakpoint_eleuth))

#residual SD around the SAR
#use sd(residuals(sar_fit)) or sigma(sar_fit) for the function from create_SAR
anole_SAR_res_sd <- sd(residuals(anole_SAR$segObj))
frog_SAR_res_sd <- sd(residuals(frog_SAR$segObj))
scal_SAR_res_sd <- sd(residuals(scal_SAR$segObj))
silv_SAR_res_sd <- sd(residuals(silver_SAR$segObj))

# From the datasets
# Caribbean: (12.1, 25.5), 41 islands (Eleutherodactylus)
log_areas_caribbean <- c(
  seq(12.1, 25.5, length.out = 41)
)

# Galapagos: (16.7, 22.2), 10 islands (Scalesia)
log_areas_galapagos <- c(
  seq(16.7, 22.2, length.out = 10)
)

# Hawaii: (19.7, 23.1), 6 islands (Silversword)
log_areas_hawaii <- c(
  seq(19.7, 23.1, length.out = 6)
)

archipelago_areas <- list(
  Caribbean = log_areas_caribbean,
  Galapagos = log_areas_galapagos,
  Hawaii    = log_areas_hawaii
)

simulate_sar <- function(log_areas, breakpoint, slope1, slope2,
                         intercept, residual_sd) {
  #expected log-richness
  log_richness <- ifelse(
    log_areas <= breakpoint,
    #small island, simple linear relationship using the the first slope
    intercept + slope1 * log_areas,
    #large island, speciation phase...using the second slope
    intercept + slope1 * breakpoint + slope2 * (log_areas - breakpoint) 
  )
  #added to the expected log-richness values
  log_richness + rnorm(length(log_areas), 0, residual_sd)
}

# Fit piecewise linear model with one breakpoint via grid search (forced)
fit_piecewise <- function(log_areas, log_richness,
                          bp_grid = seq(min(log_areas) + 0.5,
                                        max(log_areas) - 0.5,
                                        length.out = 50)) {
  if (length(log_areas) < 5) return(NA)
  
  best_rss <- Inf #baseline best residual sum of squares
  best_bp  <- NA #best breakpoint found
  
  for (bp in bp_grid) {
    #x1 equals log area for small islands and equals the breakpoint value for large islands
    x1 <- pmin(log_areas, bp)
    #x2 is zero for small islands and equals the excess area beyond the breakpoint for large islands
    x2 <- pmax(log_areas - bp, 0)
    #ordinary linear regression using x1 and x2 as predictors, free intercept
    fit <- tryCatch(
      lm(log_richness ~ x1 + x2),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    #rss to check how well the piecewise model fits at this threshold
    rss <- sum(residuals(fit)^2)
    #if this candidate produces a better fit than any previous candidate, 
    #it updates best_rss and best_bp
    if (rss < best_rss) {
      best_rss <- rss
      best_bp  <- bp
    }
  }
  best_bp
}

aic_prefers_breakpoint <- function(log_areas, log_richness,
                                   bp_grid = seq(min(log_areas) + 0.5,
                                                 max(log_areas) - 0.5,
                                                 length.out = 50)) {
  if (length(log_areas) < 5) return(FALSE)
  
  #no breakpoint model
  fit_linear <- lm(log_richness ~ log_areas)
  aic_linear <- AIC(fit_linear)
  
  #piecewise model
  best_rss <- Inf
  best_bp  <- NA
  n        <- length(log_areas)
  
  for (bp in bp_grid) {
    x1 <- pmin(log_areas, bp)
    x2 <- pmax(log_areas - bp, 0)
    fit <- tryCatch(lm(log_richness ~ x1 + x2), error = function(e) NULL)
    if (is.null(fit)) next
    rss <- sum(residuals(fit)^2)
    if (rss < best_rss) {
      best_rss <- rss
      best_bp  <- bp
    }
  }
  
  if (is.na(best_bp)) return(FALSE)
  
  #AIC manually (3 predictors + intercept = 4 params + sigma)
  k_piece <- 5
  aic_piece <- n * log(best_rss / n) + 2 * k_piece
  
  #piecewise preferred if it is better by > 2 AIC units
  (aic_linear - aic_piece) > 2
}


#Power analysis
#for each archipelago, we simulate n_sim SARs with a breakpoint at the Caribbean
#mean threshold. We then record the proportion of simulations in which a breakpoint
#is detected and favored (AIC)

#Caribbean SARs (need to adjust parameters)
# Parameters for Anolis SAR (Caribbean)
true_slope1    <- -0.01936
true_slope2    <- 1.15636
true_intercept <- 0.98023
residual_sd <- anole_SAR_res_sd

# Parameters for Eleutherodactylus SAR (Caribbean)
true_slope1    <- 0.05518
true_slope2    <- 0.81430
true_intercept <- -0.57579
residual_sd <- frog_SAR_res_sd

# Parameters for Scalesia SAR (Galapagos)
true_slope1    <- 0.38269
true_slope2    <- -0.59478
true_intercept <- -6.50040
residual_sd <- scal_SAR_res_sd

# Parameters for Silversword SAR (Hawaii)
true_slope1    <- 1.1539
true_slope2    <- -1.2114
true_intercept <- -21.5476
residual_sd <- silv_SAR_res_sd

power_results <- data.frame(
  Archipelago       = character(),
  N_islands         = integer(),
  Area_range_logm2  = character(),
  Breakpoint_in_range = character(),
  Estimated_power   = numeric(),
  stringsAsFactors  = FALSE
)

for (arch in names(archipelago_areas)) {
  areas <- archipelago_areas[[arch]]
  n_isl <- length(areas)
  area_min <- min(areas)
  area_max <- max(areas)
  bp_in_range <- (caribbean_breakpoint_mean > area_min) &
    (caribbean_breakpoint_mean < area_max)
  
  detections <- logical(n_sim)
  for (i in seq_len(n_sim)) {
    sim_richness <- simulate_sar(
      log_areas   = areas,
      breakpoint  = caribbean_breakpoint_mean,
      slope1      = true_slope1,
      slope2      = true_slope2,
      intercept   = true_intercept,
      residual_sd = residual_sd
    )
    detections[i] <- aic_prefers_breakpoint(areas, sim_richness)
  }
  
  power <- mean(detections)
  power_results <- rbind(power_results, data.frame(
    Archipelago         = arch,
    N_islands           = n_isl,
    Area_range_logkm2    = sprintf("%.1f - %.1f", area_min, area_max),
    Breakpoint_in_range = ifelse(bp_in_range, "YES", "NO — outside range"),
    Estimated_power     = round(power, 3),
    stringsAsFactors    = FALSE
  ))
}

power_results

# If power is low (<80%) for Galapagos or Hawaii AND/OR the Caribbean breakpoint
# falls outside their area range, this is strong evidence that the absence
# of a detected breakpoint reflects area range limitation, NOT biological absence.
# If power is high (>80%) and the breakpoint is within range, absence is more meaningful.

# ANOLIS PARAMS
# Archipelago N_islands Area_range_logkm2 Breakpoint_in_range Estimated_power
# 1   Caribbean        41       12.1 - 25.5                 YES               1
# 2   Galapagos        10       16.7 - 22.2                 YES               1
# 3      Hawaii         6       19.7 - 23.1                 YES               1

# FROG PARAMS
# Archipelago N_islands Area_range_logkm2 Breakpoint_in_range Estimated_power
# 1   Caribbean        41       12.1 - 25.5                 YES               1
# 2   Galapagos        10       16.7 - 22.2                 YES               1
# 3      Hawaii         6       19.7 - 23.1                 YES               1

# SCALESIA PARAMS
# Archipelago N_islands Area_range_logkm2 Breakpoint_in_range Estimated_power
# 1   Caribbean        41       12.1 - 25.5                 YES               1
# 2   Galapagos        10       16.7 - 22.2                 YES               1
# 3      Hawaii         6       19.7 - 23.1                 YES               1

# SILVERSWORD PARAMS
# Archipelago N_islands Area_range_logkm2 Breakpoint_in_range Estimated_power
# 1   Caribbean        41       12.1 - 25.5                 YES               1
# 2   Galapagos        10       16.7 - 22.2                 YES               1
# 3      Hawaii         6       19.7 - 23.1                 YES               1