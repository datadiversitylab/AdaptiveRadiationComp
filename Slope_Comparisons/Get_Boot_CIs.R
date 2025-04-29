# GOALS:
## Compare slopes of species-area relationships
## Generate bootstrap replicates for the calculation of CIs around the slope
## Output a CSV with CIs for each adaptive radiation

##### LOAD LIBRARIES #####
library(SSARP)
library(boot)

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

## Function for use in boot call: sample points in SAR
#
# Input: stand_df - standardized dataframe
#        blank - NOTHING. The boot function requires this function to have 2 parameters
# 
# Output: Slope of sampled relationship (numeric)
#
boot_func <- function(stand_df, blank){
  # Use sample() to get row numbers to construct sampled relationship
  samp <- sample(c(1:length(stand_df[,1])), size = length(stand_df[,1]), replace = TRUE)
  
  # Now that we have a list of row numbers, create new dataframe with those rows
  samp_df <- stand_df[samp,]
  
  # Use samp_df to create a new linear model
  samp_linear <- lm(y ~ x, data = samp_df)
  
  # Save coefficients
  coeff <- coef(samp_linear)
  
  # First is intercept, second is slope
  return(coeff[[2]])
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

##### BOOTSTRAP RESULTS #####
# (Assumes your environment has the standardized SAR dataframes from above)

# Create empty df to populate with CIs
ci_df <- data.frame()
# Basic_Lower Basic_Upper Percent_Lower Percent_Upper

# Cichlids
cich_boot <- boot(cich_stand, boot_func, R = 1000)
cich_basic <- boot.ci(cich_boot, type = "basic")
cich_perc <- boot.ci(cich_boot, type = "perc")

ci_df <- rbind(ci_df, c("African Cichlids", cich_basic$basic[1,4], cich_basic$basic[1,5],
                        cich_perc$percent[1,4], cich_perc$percent[1,5]))

# Phelsuma
phel_boot <- boot(phel_stand, boot_func, R = 1000)
phel_basic <- boot.ci(phel_boot, type = "basic")
phel_perc <- boot.ci(phel_boot, type = "perc")

ci_df <- rbind(ci_df, c("Phelsuma", phel_basic$basic[1,4], phel_basic$basic[1,5], 
                        phel_perc$percent[1,4], phel_perc$percent[1,5]))

# Silverswords
silver_boot <- boot(silver_stand, boot_func, R = 1000)
silver_basic <- boot.ci(silver_boot, type = "basic")
silver_perc <- boot.ci(silver_boot, type = "perc")

ci_df <- rbind(ci_df, c("Hawaiian Silverswords", silver_basic$basic[1,4], silver_basic$basic[1,5],
                        silver_perc$percent[1,4], silver_perc$percent[1,5]))

# Naesiotus
snail_boot <- boot(snail_stand, boot_func, R = 1000)
snail_basic <- boot.ci(snail_boot, type = "basic")
snail_perc <- boot.ci(snail_boot, type = "perc")

ci_df <- rbind(ci_df, c("Naesiotus Snails", snail_basic$basic[1,4], snail_basic$basic[1,5],
                        snail_perc$percent[1,4], snail_perc$percent[1,5]))

# Name ci_df columns
colnames(ci_df) <- c("Taxon", "Basic_Lower", "Basic_Upper", "Percent_Lower", "Percent_Upper")

write.csv(ci_df, "Data/All_Confidence_Intervals.csv", row.names = FALSE)
