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
  # Create empty df to append values
  jack_res <- rep(NA, length(stand_df[,1]))
  # Remove one observation (make sure each one is left out once) and then
  # get the slope between smallest and largest islands
  for(i in c(1:length(stand_df[,1]))){
    # Remove ith row
    out_df <- stand_df[-i,]
    
    # Find species richness value for smallest island
    small_sp <- out_df[which(out_df$x == min(out_df$x)),]
    
    # Find species richness value for largest island
    large_sp <- out_df[which(out_df$x == max(out_df$x)),]
    
    # Get slope between those two points?
    # y2-y1 / x2-x1
    jack_res[i] <- (large_sp$y - small_sp$y) / (large_sp$x - small_sp$x)
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
phel_jack <- stand_jack(phel_stand)
phel_CI <- quantile(phel_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Phelsuma", phel_CI[1], phel_CI[2]))

# African Cichlids
cich_jack <- stand_jack(cich_stand)
cich_CI <- quantile(cich_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("African Cichlids", cich_CI[1], cich_CI[2]))

# Hawaiian Silverswords
silver_jack <- stand_jack(silver_stand)
silver_CI <- quantile(silver_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Hawaiian Silverswords", silver_CI[1], silver_CI[2]))

# Naesiotus Snails
snail_jack <- stand_jack(snail_stand)
snail_CI <- quantile(snail_jack, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Naesiotus Snails", snail_CI[1], snail_CI[2]))

# Anolis lizards
anole_jack <- stand_jack(anole_stand)
anole_CI <- quantile(anole_jack, c(0.025, 0.975))


# Name ci_df columns
colnames(ci_df) <- c("Taxon", "2.5%", "97.5%")

write.csv(ci_df, "Data/Modified_Jackknife_Confidence_Intervals.csv", row.names = FALSE)
