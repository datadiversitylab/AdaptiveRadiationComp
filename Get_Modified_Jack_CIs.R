# GOAL: Re-write the jackknife analysis to:
## Consider the rate of change between smallest and largest islands

##### LOAD LIBRARIES #####
library(SSARP)
library(segmented)

##### FUNCTIONS #####

## Transform values to range from 0 to 1
# Code from https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
#
# Input: numeric vector
#
# Output: numeric vector
#
range01 <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

## Standardize x and y values of a species-area relationship to be between 0 and 1
#
# Input: areas - dataframe with species names and land areas
#        npsi - number of breakpoints for the SAR
#
# Output; dataframe with standardized x and y values for SAR comparison
#
# Requires: SSARP R package
#
standardize <- function(areas, npsi){
  # Create SAR
  SAR <- SSARP::SARP(areas, npsi = npsi)
  
  # Extract data used to plot the SAR
  agg_df <- SAR$aggDF
  
  # Change range of both axes to be between 0 and 1
  agg_df$x <- range01(agg_df$x)
  agg_df$y <- range01(agg_df$y)
  
  # Return standardized df
  return(agg_df)
  
}

## Generate jackknife replicates for the SAR linear model
# 
# Input: stand_df - the standardized version of the species-area relationship data
#        npsi - the number of breakpoints in the best-fit SAR model
#
# Output: dataframe with jackknife resampled slopes and intercepts for the SAR
# 
stand_jack <- function(stand_df, npsi){
  # Create empty vector to append values
  jack_res <- rep(NA, length(stand_df[,1]))
  
  # Create dataframes for predicting species richness at:
  # Smallest island
  small_df <- data.frame(x = 0)
  # Largest island
  large_df <- data.frame(x = 1)
  
  # Remove one observation (make sure each one is left out once) and then
  # get the slope between smallest and largest islands
  for(i in c(1:length(stand_df[,1]))){
    # Remove ith row
    out_df <- stand_df[-i,]
    
    # Create new linear model for prediction using out_df
    new_model <- lm(y ~ x, data = out_df)
    
    # If npsi > 0, create segmented model
    if(npsi > 0){
      new_model <- segmented(new_model, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE))
    }
    
    # Now predict species richness at largest and smallest island using the new model
    small <- predict(new_model, small_df)
    large <- predict(new_model, large_df)
    
    # Get slope between the smallest and largest points
    # y2-y1 / x2-x1
    jack_res[i] <- (large[[1]] - small[[1]]) / (1 - 0)
  }
  
  return(jack_res)
}

##### STANDARDIZE SAR #####

## Phelsuma ##
phel_areas <- read.csv("Data/Phelsuma_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
phel_stand <- standardize(phel_areas, 0)

# Calculate linear model with standardized data
phel_linear <- lm(y ~ x, data = phel_stand)

# Plot standardized SAR
plot(phel_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(phel_linear)

## African Cichlids ##
cich_areas <- read.csv("Data/Cichlid_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
cich_stand <- standardize(cich_areas, 0)

# Calculate linear model with standardized data
cich_linear <- lm(y ~ x, data = cich_stand)

# Plot standardized SAR
plot(cich_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(cich_linear)

## Hawaiian Silverswords ##
silver_areas <- read.csv("Data/Silversword_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
silver_stand <- standardize(silver_areas, 0)

# Calculate linear model with standardized data
silver_linear <- lm(y ~ x, data = silver_stand)

# Plot standardized SAR
plot(silver_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(silver_linear)

## Naesiotus Snails ##
snail_areas <- read.csv("Data/Naesiotus_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
snail_stand <- standardize(snail_areas, 0)

# Calculate linear model with standardized data
snail_linear <- lm(y ~ x, data = snail_stand)

# Plot standardized SAR
plot(snail_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(snail_linear)

## Anolis lizards ##
anole_areas <- read.csv("Data/Anolis_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
anole_stand <- standardize(anole_areas, 1)

# Plot standardized SAR
# For Anolis, this is a segmented regression
# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = anole_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(anole_stand$x, anole_stand$y, pch = 19)

##### JACKKNIFE RESULTS #####
# (Assumes your environment has the standardized SAR dataframes from above)
# Use jackknife resampling to estimate slope between smallest and largest island
# Get 95% confidence interval through the percentile method (find 2.5 and 97.5 percentiles)

# Create empty df to populate with CIs
ci_df <- data.frame()

# Phelsuma
phel_jack <- stand_jack(phel_stand, 0)
phel_CI <- quantile(phel_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Phelsuma", phel_CI[1], phel_CI[2]))

# African Cichlids
cich_jack <- stand_jack(cich_stand, 0)
cich_CI <- quantile(cich_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("African Cichlids", cich_CI[1], cich_CI[2]))

# Hawaiian Silverswords
silver_jack <- stand_jack(silver_stand, 0)
silver_CI <- quantile(silver_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Hawaiian Silverswords", silver_CI[1], silver_CI[2]))

# Naesiotus Snails
snail_jack <- stand_jack(snail_stand, 0)
snail_CI <- quantile(snail_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Naesiotus Snails", snail_CI[1], snail_CI[2]))

# Anolis lizards
anole_jack <- stand_jack(anole_stand, 1)
anole_CI <- quantile(anole_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Anolis Lizards", anole_CI[1], anole_CI[2]))


# Name ci_df columns
colnames(ci_df) <- c("Taxon", "2.5%", "97.5%")

write.csv(ci_df, "Data/Modified_Jackknife_Confidence_Intervals.csv", row.names = FALSE)

##### JACKKNIFE COMPARISON #####
# Generate the same style of confidence intervals around the slope for the
## comparison genera

# Create empty df to populate with CIs
ci_df <- data.frame()

## Madagascar Chameleons
furci_areas <- read.csv("Data/Furcifer_areas.csv")

# NOTE: best-fit SAR has 1 breakpoint, but it errors during the jackknife
# Using linear model instead

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
furci_stand <- standardize(furci_areas, 0)

# Calculate linear model with standardized data
furci_linear <- lm(y ~ x, data = furci_stand)

# Plot standardized SAR
plot(furci_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(furci_linear)

# Now conduct jackknife
furci_jack <- stand_jack(furci_stand, 0)
furci_CI <- quantile(furci_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Madagascar Chameleons", furci_CI[1], furci_CI[2]))


## African Lake Fish
enter_areas <- read.csv("Data/Enteromius_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
enter_stand <- standardize(enter_areas, 1) # Best-fit SAR has 1 breakpoint

# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = enter_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(enter_stand$x, enter_stand$y, pch = 19)

# Now conduct jackknife
enter_jack <- stand_jack(enter_stand, 1) # 1 breakpoint
enter_CI <- quantile(enter_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("African Enteromius", enter_CI[1], enter_CI[2]))


## Hawaiian Acacia
acacia_areas <- read.csv("Data/Acacia_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
acacia_stand <- standardize(acacia_areas, 1) # Best-fit SAR has 1 breakpoint

# Run a linear model on the data to use in creating segmented/breakpoint regression
linear <- lm(y ~ x, data = acacia_stand)

seg <- segmented(linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))

# Plot the breakpoint regression line
plot(seg, rug = FALSE,
     xlim = c(0,1),
     ylim = c(0,1),
     ylab = "Log Number of Species",
     xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship")
# Add the points
points(acacia_stand$x, acacia_stand$y, pch = 19)

# Now conduct jackknife
acacia_jack <- stand_jack(acacia_stand, 1) # 1 breakpoint
acacia_CI <- quantile(acacia_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Hawaiian Acacia", acacia_CI[1], acacia_CI[2]))


## Galapagos Opuntia
cactus_areas <- read.csv("Data/Opuntia_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
cactus_stand <- standardize(cactus_areas, 0)

# Calculate linear model with standardized data
cactus_linear <- lm(y ~ x, data = cactus_stand)

# Plot standardized SAR
plot(cactus_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(cactus_linear)

# Now conduct jackknife
cactus_jack <- stand_jack(cactus_stand, 0)
cactus_CI <- quantile(cactus_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Galapagos Opuntia", cactus_CI[1], cactus_CI[2]))


## Caribbean Tropidophis
trop_areas <- read.csv("Data/Tropidophis_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
trop_stand <- standardize(trop_areas, 0)

# Calculate linear model with standardized data
trop_linear <- lm(y ~ x, data = trop_stand)

# Plot standardized SAR
plot(trop_stand,
     ylab = "Standardized Log Number of Species",
     xlab = expression(paste("Standardized Log Island Area (", "m"^"2", ")")),
     main = "Species-Area Relationship",
     pch = 16)
abline(trop_linear)

# Now conduct jackknife
trop_jack <- stand_jack(trop_stand, 0)
trop_CI <- quantile(trop_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Caribbean Tropidophis", trop_CI[1], trop_CI[2]))

# Name ci_df columns
colnames(ci_df) <- c("Taxon", "2.5%", "97.5%")

write.csv(ci_df, "Data/Comparison_Jackknife_Confidence_Intervals.csv", row.names = FALSE)
