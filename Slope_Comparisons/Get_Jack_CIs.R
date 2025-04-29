# GOALS:
## Compare slopes of species-area relationships
## Generate jackknife replicates for the calculation of CIs around the slope
## Output a CSV with CIs for each adaptive radiation

##### LOAD LIBRARIES #####
library(SSARP)

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
#
# Output: dataframe with jackknife resampled slopes and intercepts for the SAR
# 
stand_jack <- function(stand_df){
  # Create empty df to append values
  jack_df <- data.frame()
  # Remove one observation (make sure each one is left out once) and then
  # re-make the linear model and save the slope and intercept
  for(i in c(1:length(stand_df[,1]))){
    # Remove ith row
    out_df <- stand_df[-i,]
    
    # Re-make linear model
    stand_linear <- lm(y ~ x, data = out_df)
    
    # Save coefficients
    coeff <- coef(stand_linear)
    
    # First is intercept, second is slope
    jack_df <- rbind(jack_df, c(coeff[[1]], coeff[[2]]))
  }
  colnames(jack_df) <- c("intercept", "slope")
  
  return(jack_df)
}

##### STANDARDIZE SAR #####
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

##### JACKKNIFE RESULTS #####
# (Assumes your environment has the standardized SAR dataframes from above)
# Use jackknife resampling to estimate slopes and intercepts for each SAR
# Get 95% confidence interval through the percentile method (find 2.5 and 97.5 percentiles)

# Create empty df to populate with CIs
ci_df <- data.frame()

# Phelsuma
phel_jack <- stand_jack(phel_stand)
phel_CI <- quantile(phel_jack$slope, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Phelsuma", phel_CI[[1]], phel_CI[[2]]))

# African Cichlids
cich_jack <- stand_jack(cich_stand)
cich_CI <- quantile(cich_jack$slope, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("African Cichlids", cich_CI[[1]], cich_CI[[2]]))

# Hawaiian Silverswords
silver_jack <- stand_jack(silver_stand)
silver_CI <- quantile(silver_jack$slope, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Hawaiian Silverswords", silver_CI[[1]], silver_CI[[2]]))

# Naesiotus Snails
snail_jack <- stand_jack(snail_stand)
snail_CI <- quantile(snail_jack$slope, c(0.025, 0.975))
ci_df <- rbind(ci_df, c("Naesiotus Snails", snail_CI[[1]], snail_CI[[2]]))

# Name ci_df columns
colnames(ci_df) <- c("Taxon", "2.5%", "97.5%")

write.csv(ci_df, "Data/Jackknife_Confidence_Intervals.csv", row.names = FALSE)
