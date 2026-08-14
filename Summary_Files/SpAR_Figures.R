# (Edited from Figure2.R) #

# HOW TO USE: Read in .rds files for desired epsilon value, then jump to
#   plot code. If you want to generate plots for all three epsilon (e) values,
#   overwrite *_SpAR objects with those that correspond with desired e values.

library(ggplot2)
library(cowplot)

##### SpARs with epsilon = 0 #####

anole_SpAR <- readRDS("Caribbean/Data/anolis_SpAR_e0.rds")
frog_SpAR <- readRDS("Caribbean/Data/frog_SpAR_e0.rds")

finch_SpAR <- readRDS("Galapagos/Data/finch_SpAR_e0.rds")
scal_SpAR <- readRDS("Galapagos/Data/scalesia_SpAR_e0.rds")

silver_SpAR <- readRDS("Hawaiian/Data/silver_SpAR_e0.rds")
spider_SpAR <- readRDS("Hawaiian/Data/spider_SpAR_e0.rds")

# ~~~~~~~~~

##### SpARs with epsilon = 0.5 #####
anole_SpAR <- readRDS("Caribbean/Data/anolis_SpAR_e05.rds")
frog_SpAR <- readRDS("Caribbean/Data/frog_SpAR_e05.rds")

finch_SpAR <- readRDS("Galapagos/Data/finch_SpAR_e05.rds")
scal_SpAR <- readRDS("Galapagos/Data/scalesia_SpAR_e05.rds")

silver_SpAR <- readRDS("Hawaiian/Data/silver_SpAR_e05.rds")
spider_SpAR <- readRDS("Hawaiian/Data/spider_SpAR_e05.rds")

# ~~~~~~~~~

##### SpARs with epsilon = 0.9 #####
anole_SpAR <- readRDS("Caribbean/Data/anolis_spar_e09.rds")
frog_SpAR <- readRDS("Caribbean/Data/frog_SpAR_e09.rds")

finch_SpAR <- readRDS("Galapagos/Data/finch_SpAR_e09.rds")
scal_SpAR <- readRDS("Galapagos/Data/scalesia_SpAR_e09.rds")

silver_SpAR <- readRDS("Hawaiian/Data/silver_SpAR_e09.rds")
spider_SpAR <- readRDS("Hawaiian/Data/spider_SpAR_e09.rds")

##### Plotting SpARs #####

# IDEA: Just like when making marginal effects plots, we can create a dataframe
#       that includes a sequence of the value on the x-axis, and predict what
#       the value on the y-axis should be for each.

## ANOLES
# Create a new dataframe with a sequence of area values
newdat_anole <- data.frame(x = seq(min(anole_SpAR$aggDF[,1]),
                                   max(anole_SpAR$aggDF[,1]),
                                   length.out = 100))

# Use the segmented regression model to predict along the new data
newdat_anole$fit <- predict(anole_SpAR$segObj, newdata = newdat_anole)

# Original Anole data
anole_dat <- data.frame(
  x = anole_SpAR$aggDF[,1],
  y = anole_SpAR$aggDF[,2]
)

## FROGS
newdat_frog <- data.frame(x = seq(min(frog_SpAR$aggDF[,1]),
                                  max(frog_SpAR$aggDF[,1]),
                                  length.out = 100))
# Predict for frogs
newdat_frog$fit <- predict(frog_SpAR$segObj, newdata = newdat_frog)

## Anole + Frog SpAR
af_SpAR <- ggplot(anole_SpAR$aggDF, aes(x = x, y = y)) + 
  geom_point(aes(color = "Anolis")) +
  geom_line(data = newdat_anole, aes(x = x, y = fit, color = "Anolis"), linewidth = 1) +
  # Frog data
  geom_point(data = frog_SpAR$aggDF, aes(x = x, y = y, color = "Eleutherodactylus")) +
  geom_line(data = newdat_frog, aes(x = x, y = fit, color = "Eleutherodactylus"), linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Diversification Rate)") +
  # Add a legend
  scale_color_manual(
    name = NULL,
    values = c(
      "Anolis" = "#E69F00",
      "Eleutherodactylus" = "#56B4E9"),
    labels = c(
      expression(italic("Anolis")),
      expression(italic("Eleutherodactylus")))) +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic() +
  theme(text = element_text(size = 10))

## FINCHES
# This is a linear regression, but may as well follow the same methods
# Finch newdat
newdat_finch <- data.frame(x = seq(min(finch_SpAR$aggDF[,1]),
                                   max(finch_SpAR$aggDF[,1]),
                                   length.out = 100))
# Predict for finches
newdat_finch$fit <- predict(finch_SpAR$linObj, newdata = newdat_finch)

## SCALESIA
# Scalesia newdat
newdat_scal <- data.frame(x = seq(min(scal_SpAR$aggDF[,1]),
                                  max(scal_SpAR$aggDF[,1]),
                                  length.out = 100))
# Predict for Scalesia
newdat_scal$fit <- predict(scal_SpAR$linObj, newdata = newdat_scal)

## Finch + Scalesia SpAR
fs_SpAR <- ggplot(finch_SpAR$aggDF, aes(x = x, y = y)) + 
  geom_point(aes(color = "finch")) +
  geom_line(data = newdat_finch, aes(x = x, y = fit, color = "finch"), linewidth = 1) +
  # Scalesia data
  geom_point(data = scal_SpAR$aggDF, aes(x = x, y = y, color = "scal")) +
  geom_line(data = newdat_scal, aes(x = x, y = fit, color = "scal"), linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Diversification Rate)") +
  # Add a legend
  scale_color_manual(
    name = NULL,
    values = c(
      "finch" = "#009E73",
      "scal" = "#F0E442"),
    labels = c(
      "Finches",
      expression(italic("Scalesia")))) +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic() +
  theme(text = element_text(size = 10))

## SILVERSWORDS
# This is a linear regression, but may as well follow the same methods
# Silver newdat
newdat_silver <- data.frame(x = seq(min(silver_SpAR$aggDF[,1]),
                                    max(silver_SpAR$aggDF[,1]),
                                    length.out = 100))
# Predict for silverswords
newdat_silver$fit <- predict(silver_SpAR$linObj, newdata = newdat_silver)

## TETRAGNATHA
# Spider newdat
newdat_spider <- data.frame(x = seq(min(spider_SpAR$aggDF[,1]),
                                    max(spider_SpAR$aggDF[,1]),
                                    length.out = 100))
# Predict for spiders
newdat_spider$fit <- predict(spider_SpAR$linObj, newdata = newdat_spider)

## Silversword + Tetragnatha SpAR
st_SpAR <- ggplot(silver_SpAR$aggDF, aes(x = x, y = y)) + 
  geom_point(aes(color = "silver")) +
  geom_line(data = newdat_silver, aes(x = x, y = fit, color = "silver"), linewidth = 1) +
  # Spider data
  geom_point(data = spider_SpAR$aggDF, aes(x = x, y = y, color = "spider")) +
  geom_line(data = newdat_spider, aes(x = x, y = fit, color = "spider"), linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Diversification Rate)") +
  # Add a legend
  scale_color_manual(
    name = NULL,
    values = c(
      "silver" = "#D55E00",
      "spider" = "#0072B2"),
    labels = c(
      "Silverswords",
      expression(italic("Tetragnatha")))) +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic() +
  theme(text = element_text(size = 10))

#### Plot all SpARs in a row ####
# af_SpAR = anole frog
# fs_SpAR = finch scalesia
# st_SpAR = silversword tetragnatha

# Add plot margins
margins <- theme(
  plot.margin = margin(t = 15, r = 5, b = 5, l = 5)
)

af_SpAR <- af_SpAR + margins
fs_SpAR <- fs_SpAR + margins
st_SpAR <- st_SpAR + margins


# Add the okina for Hawaii (\u02BB)
library(showtext)
# Maybe I should use a different font...
font_add_google("Noto Sans", "noto")
showtext_auto()

# Set the font theme outside of the grid so it applies to everything
theme_set(theme_minimal(base_family = "noto"))

# cairo_pdf("Figure3.pdf", width = 12, height = 4)
p <- plot_grid(af_SpAR,
          fs_SpAR,
          st_SpAR,
          align = "h",
          ncol = 3,
          labels = c("a) Caribbean", "b) Galápagos", "c) Hawai\u02BBi"),
          label_x = 0,
          label_y = 1.03
)
# Make sure that the labels don't get cut off - use draw_plot to place it with
#     some white space
ggdraw() +
  draw_plot(p, y = -0.02, height = 0.98)

# dev.off()
