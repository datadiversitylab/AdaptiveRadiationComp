## MAYBE WE SHOULD USE ANOVA INSTEAD??

# Visually compare SARs between target adaptive radiation and comparison

## MADAGASCAR
# Phelsuma Geckos
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
     pch = 16, col = "green")
abline(phel_linear, col = "green")

# Furcifer Chameleons
furci_areas <- read.csv("Data/Furcifer_areas.csv")

# NOTE: best-fit SAR has 1 breakpoint, but it errors during the jackknife
# Using linear model instead

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
furci_stand <- standardize(furci_areas, 0)

# Calculate linear model with standardized data
furci_linear <- lm(y ~ x, data = furci_stand)

# Add furci info to the existing plot?
points(furci_stand, pch = 16, col = "blue")
abline(furci_linear, col = "blue")

legend("topleft", legend = c("Phelsuma", "Furcifer"), fill = c("green", "blue"))

## AFRICAN LAKES
# Cichlid fish
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
     pch = 16, col = "orange")
abline(cich_linear, col = "orange")

# Now add Enteromius info
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
points(enter_stand$x, enter_stand$y, pch = 19, col = "red")

# Add the cichlid info last I guess
points(cich_stand$x, cich_stand$y, pch = 19, col = "orange")
abline(cich_linear, col = "orange")

legend("topleft", legend = c("Cichlids", "Enteromius"), fill = c("orange", "red"))

## HAWAIIAN ISLANDS
# Hawaiian Silverswords
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
     pch = 16, col = "gray")
abline(silver_linear, col = "gray")

# Hawaiian Acacia
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
points(acacia_stand$x, acacia_stand$y, pch = 19, col = "red")

# Okay now add points for silverswords
points(silver_stand$x, silver_stand$y, pch = 19, col = "black")
abline(silver_linear, col = "black")

legend("topleft", legend = c("Silverswords", "Acacia"), fill = c("black", "red"))

## GALAPAGOS ISLANDS
# Naesiotus Snails
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
     pch = 16, col = "brown")
abline(snail_linear, col = "brown")

# Galapagos Opuntia
cactus_areas <- read.csv("Data/Opuntia_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
cactus_stand <- standardize(cactus_areas, 0)

# Calculate linear model with standardized data
cactus_linear <- lm(y ~ x, data = cactus_stand)

# Add cactus info to plot
abline(cactus_linear, col = "green")
points(cactus_stand$x, cactus_stand$y, pch = 16, col = "green")

legend("topleft", legend = c("Naesiotus", "Opuntia"), fill = c("brown", "green"))


## CARIBBEAN ISLANDS
# Anolis
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
points(anole_stand$x, anole_stand$y, pch = 19, col = "red")

# Tropidophis
trop_areas <- read.csv("Data/Tropidophis_areas.csv")

# Get standardized version of SAR
# (This will automatically output the regular SAR in Plots)
trop_stand <- standardize(trop_areas, 0)

# Calculate linear model with standardized data
trop_linear <- lm(y ~ x, data = trop_stand)

# Add points and line
points(trop_stand$x, trop_stand$y, pch = 16, col = "blue")
abline(trop_linear, col = "blue")

legend("topleft", legend = c("Anolis", "Tropidophis"), fill = c("red", "blue"))
