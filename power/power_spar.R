# If a breakpoint existed at the Caribbean threshold, 
# would we detect it in each archipelago given its real 
# island areas and sample size?
set.seed(42)
library(segmented)

# Load SpAR objects
anole_SAR <- readRDS("Caribbean/Data/anolis_spar_e0.rds")
frog_SAR <- readRDS("Caribbean/Data/frog_SpAR_e0.rds")

# Breakpoint to test
caribbean_breakpoint_anolis <- 22.3
caribbean_breakpoint_eleuth <- 21.7
breakpoint   <- mean(c(caribbean_breakpoint_anolis, caribbean_breakpoint_eleuth))

# Slopes and intercept (Caribbean)
resid_sd  <- mean(c(sd(residuals(anole_SAR$segObj)), sd(residuals(frog_SAR$segObj))))
slope1    <- mean(c(slope(anole_SAR$segObj)[[1]][1,1], slope(frog_SAR$segObj)[[1]][1,1]))
slope2    <- mean(c(slope(anole_SAR$segObj)[[1]][2,1], slope(frog_SAR$segObj)[[1]][2,1]))
intercept <- mean(c(anole_SAR$segObj[[1]][1], frog_SAR$segObj[[1]][1]))

# Areas
galapagosarea <- read.csv("Galapagos/galap_total.csv")
galapagosarea <- galapagosarea[-duplicated(galapagosarea$name), "area"]

hawaiiarea <- read.csv("Hawaiian/hawaii_total.csv")
hawaiiarea <- hawaiiarea[-duplicated(hawaiiarea$name),"area"]

caribbeanarea <- read.csv("Caribbean/carib_islands.csv")
caribbeanarea <- caribbeanarea$area

areas <- list(
  Caribbean = log(caribbeanarea),  
  Galapagos = log(galapagosarea),  
  Hawaii    = log(hawaiiarea)  
)

# brief check
sapply(areas, function(a) c(below = sum(a < breakpoint), above = sum(a > breakpoint)))

# Number of simulations per archipelago
n_sim <- 5000

# Simulate log richness from a two-slope SAR w/ noise
simulate_richness <- function(log_area, bp, slope1, slope2, intercept, resid_sd) {
  expected <- ifelse(log_area <= bp,
                     intercept + slope1 * log_area,
                     intercept + slope1 * bp + slope2 * (log_area - bp))
  expected + rnorm(length(log_area), 0, resid_sd)
}

# Fit a two-slope SAR with fixed breakpoint
fit_at_breakpoint <- function(log_area, log_rich, bp) {
  x1 <- pmin(log_area, bp)
  x2 <- pmax(log_area - bp, 0)
  lm(log_rich ~ x1 + x2)
}

# Ask whether a breakpoint model fits better than a straight line
detect_breakpoint <- function(log_area, log_rich, n_grid = 50) {
  grid <- seq(min(log_area) + 0.5, max(log_area) - 0.5, length.out = n_grid)
  aic_grid <- sapply(grid, function(bp) AIC(fit_at_breakpoint(log_area, log_rich, bp)))
  
  # Penalize by 2 since the breakpoint is itself a parameter
  aic_break <- min(aic_grid) + 2
  aic_line  <- AIC(lm(log_rich ~ log_area))
  
  list(detected   = (aic_line - aic_break) > 2,
       breakpoint = grid[which.min(aic_grid)])
}

# Run the simulation n times and return the proportion detected
estimate_power <- function(log_area, bp, slope1, slope2, intercept, resid_sd, n_sim) {
  hits <- replicate(n_sim, {
    sim <- simulate_richness(log_area, bp, slope1, slope2, intercept, resid_sd)
    detect_breakpoint(log_area, sim)$detected
  })
  mean(hits)
}


# Run analyses
results <- data.frame(
  archipelago = names(areas),
  n_islands   = sapply(areas, length),
  area_min    = round(sapply(areas, min), 1),
  area_max    = round(sapply(areas, max), 1),
  bp_in_range = sapply(areas, function(a) breakpoint > min(a) & breakpoint < max(a)),
  power       = NA,
  row.names   = NULL
)

for (i in seq_len(nrow(results))) {
  results$power[i] <- estimate_power(
    log_area  = areas[[i]],
    bp        = breakpoint,
    slope1    = slope1,
    slope2    = slope2,
    intercept = intercept,
    resid_sd  = resid_sd,
    n_sim     = n_sim
  )
}

print(results)

write.csv(results, "power/power_spar.csv")

# bp_in_range = FALSE means the archipelago has no islands near the threshold.
# power < 0.8 means underpowered.
# power > 0.8 with the breakpoint in range implies that
# the null result is real and this is a finding.
