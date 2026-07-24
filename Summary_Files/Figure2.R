# Testing shapefiles with ggplot maps
# Goal: Color islands by whether one or two of the taxa are on them

library(ggplot2)
library(cowplot)
library(segmented)

##### Hawaii #####
# Using the hawaii_wgs84 object from Hawaiian_Elevation.R
hawaii <- readRDS("Hawaiian/Data/hawaii_wgs84.rds")

# Create a new column that says whether the island includes taxon 1, taxon 2, or both
# Read in lineage data
spider_dat <- read.csv("Hawaiian/Data/Tetragnatha_Areas.csv")
silver_dat <- read.csv("Hawaiian/Data/Silversword_Areas.csv")

# Determine what islands are represented in each taxon
spider_islands <- unique(spider_dat$Second)
silver_islands <- unique(silver_dat$Third)

# Now fill in a column as part of the sf object
hawaii$presence <- rep(NA, length(hawaii$water))

for(i in c(1:length(hawaii$isle))){
  # Both are on the island
  if(hawaii$isle[i] %in% spider_islands && hawaii$isle[i] %in% silver_islands){
    hawaii$presence[i] <- "Both"
  }
  # Only spiders are on the island
  if(hawaii$isle[i] %in% spider_islands && !(hawaii$isle[i] %in% silver_islands)){
    hawaii$presence[i] <- "Tetragnatha"
  }
  # Only silverswords are on the island
  if(!(hawaii$isle[i] %in% spider_islands) && hawaii$isle[i] %in% silver_islands){
    hawaii$presence[i] <- "Silverswords"
  }
}

# Finally, plot the islands with the appropriate colors
# "Both" = "#CC79A7"
hawaii_plot <- ggplot(hawaii) +
  geom_sf(aes(fill = presence), show.legend = TRUE) +
  scale_fill_manual(
    values = c(
      "Tetragnatha" = "#0072B2",
      "Silverswords" = "#D55E00",
      "Both" = "#A7005C"),
    labels = c(
      expression(italic("Tetragnatha")),
      "Silverswords",
      "Both",
      "NA"),
    # Make sure all categories show up, even if not in this plot
    limits = c("Tetragnatha", "Silverswords", "Both"),
    drop = FALSE,
    # Remove NA from the legend
    na.translate = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())

##### Galapagos #####
# Using the galap_final object from Galapagos_Elevation.R
galap <- readRDS("Galapagos/Data/galap_final.rds")

# Create a new column that says whether the island includes taxon 1, taxon 2, or both
# Read in lineage data
finch_dat <- read.csv("Galapagos/Data/finch_pam_areas_2.csv")
scal_dat <- read.csv("Galapagos/Data/Scalesia_Areas.csv")

# Determine what islands are represented in each taxon
finch_islands <- unique(finch_dat$Third)
scal_islands <- unique(scal_dat$Second)

# Now fill in a column as part of the sf object
galap$presence <- rep(NA, length(galap$nombre))

for(i in c(1:length(galap$nombre))){
  # Both are on the island
  if(galap$nombre[i] %in% finch_islands && galap$nombre[i] %in% scal_islands){
    galap$presence[i] <- "Both"
  }
  # Only finches are on the island
  if(galap$nombre[i] %in% finch_islands && !(galap$nombre[i] %in% scal_islands)){
    galap$presence[i] <- "Finches"
  }
  # Only Scalesia are on the island
  if(!(galap$nombre[i] %in% finch_islands) && galap$nombre[i] %in% scal_islands){
    galap$presence[i] <- "Scalesia"
  }
}

# Finally, plot the islands with the appropriate colors
galap_plot <- ggplot(galap) +
  geom_sf(aes(fill = presence), show.legend = TRUE) +
  scale_fill_manual(values = c(
    "Finches" = "#009E73",
    "Scalesia" = "#F0E442",
    "Both" = "#A7005C"),
  labels = c(
    "Finches",
    expression(italic("Scalesia")),
    "Both"),
  # Make sure all categories show up, even if not in this plot
  limits = c("Finches", "Scalesia", "Both"),
  drop = FALSE,
  na.translate = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())

##### Caribbean #####
# Using the carib_final object from Caribbean_Elevation.R
carib <- readRDS("Caribbean/Data/carib_final.rds")

# Create a new column that says whether the island includes taxon 1, taxon 2, or both
# Read in lineage data
# Anole occurrences with native/nonnative designation
anole_dat <- read.csv("Caribbean/anole_filter_testing.csv")
# Include only natively occurring records
anole_dat <- anole_dat[which(anole_dat$native == 1),]

frog_dat <- read.csv("Caribbean/Data/Eleutherodactylus_Curated.csv")

# Determine what islands are represented in each taxon
anole_islands <- unique(anole_dat$island)
frog_islands <- unique(frog_dat$First)

# Now fill in a column as part of the sf object
carib$presence <- rep(NA, length(carib$Name_USGSO))

for(i in c(1:length(carib$Name_USGSO))){
  # Both are on the island
  if(carib$Name_USGSO[i] %in% anole_islands && carib$Name_USGSO[i] %in% frog_islands){
    carib$presence[i] <- "Both"
  }
  # Only Anoles are on the island
  if(carib$Name_USGSO[i] %in% anole_islands && !(carib$Name_USGSO[i] %in% frog_islands)){
    carib$presence[i] <- "Anolis"
  }
  # Only frogs are on the island
  if(!(carib$Name_USGSO[i] %in% anole_islands) && carib$Name_USGSO[i] %in% frog_islands){
    carib$presence[i] <- "Eleutherodactylus"
  }
}

# Finally, plot the islands with the appropriate colors
carib_plot <- ggplot(carib) +
  geom_sf(aes(fill = presence), show.legend = TRUE) +
  scale_fill_manual(values = c(
    "Anolis" = "#E69F00",
    "Eleutherodactylus" = "#56B4E9",
    "Both" = "#A7005C"),
  labels = c(
    expression(italic("Anolis")),
    expression(italic("Eleutherodactylus")),
    "Both"),
  # Make sure all categories show up, even if not in this plot
  limits = c("Anolis", "Eleutherodactylus", "Both"),
  drop = FALSE,
  # Remove NA from the legend
  na.translate = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())


##### Plot segmented SARs in ggplot2
# Read in SAR objects
anole_SAR <- readRDS("Caribbean/Data/anole_SAR.rds")
frog_SAR <- readRDS("Caribbean/Data/frog_SAR.rds")
finch_SAR <- readRDS("Galapagos/Data/finch_SAR.rds")
scal_SAR <- readRDS("Galapagos/Data/scal_SAR.rds")
silver_SAR <- readRDS("Hawaiian/Data/silver_SAR.rds")
spider_SAR <- readRDS("Hawaiian/Data/spider_SAR.rds")

# IDEA: Just like when making marginal effects plots, we can create a dataframe
#       that includes a sequence of the value on the x-axis, and predict what
#       the value on the y-axis should be for each. This should fix the problem
#       I had before with trying to use broken.line like StackOverflow suggested

## ANOLES
# Create a new dataframe with a sequence of area values
newdat_anole <- data.frame(x = seq(min(anole_SAR$aggDF[,1]),
                              max(anole_SAR$aggDF[,1]),
                              length.out = 100))

# Use the segmented regression model to predict along the new data
newdat_anole$fit <- predict(anole_SAR$segObj, newdata = newdat_anole)

# Original Anole data
anole_dat <- data.frame(
  x = anole_SAR$aggDF[,1],
  y = anole_SAR$aggDF[,2]
)

## FROGS
newdat_frog <- data.frame(x = seq(min(frog_SAR$aggDF[,1]),
                                  max(frog_SAR$aggDF[,1]),
                                  length.out = 100))
# Predict for frogs
newdat_frog$fit <- predict(frog_SAR$segObj, newdata = newdat_frog)

## Anole + Frog SAR
af_SAR <- ggplot(anole_SAR$aggDF, aes(x = x, y = y)) + 
  geom_point(color = "#E69F00") +
  geom_line(data = newdat_anole, aes(x = x, y = fit), color = "#E69F00", linewidth = 1) +
  # Frog data
  geom_point(data = frog_SAR$aggDF, aes(x = x, y = y), color = "#56B4E9") +
  geom_line(data = newdat_frog, aes(x = x, y = fit), color = "#56B4E9", linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Number of Species)") +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic()

## FINCHES
# This is a linear regression, but may as well follow the same methods
# Finch newdat
newdat_finch <- data.frame(x = seq(min(finch_SAR$aggDF[,1]),
                                  max(finch_SAR$aggDF[,1]),
                                  length.out = 100))
# Predict for finches
newdat_finch$fit <- predict(finch_SAR$linObj, newdata = newdat_finch)

## SCALESIA
# Scalesia newdat
newdat_scal <- data.frame(x = seq(min(scal_SAR$aggDF[,1]),
                                    max(scal_SAR$aggDF[,1]),
                                    length.out = 100))
# Predict for Scalesia
newdat_scal$fit <- predict(scal_SAR$segObj, newdata = newdat_scal)

## Finch + Scalesia SAR
fs_SAR <- ggplot(finch_SAR$aggDF, aes(x = x, y = y)) + 
  geom_point(color = "#009E73") +
  geom_line(data = newdat_finch, aes(x = x, y = fit), color = "#009E73", linewidth = 1) +
  # Scalesia data
  geom_point(data = scal_SAR$aggDF, aes(x = x, y = y), color = "#F0E442") +
  geom_line(data = newdat_scal, aes(x = x, y = fit), color = "#F0E442", linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Number of Species)") +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic()

## SILVERSWORDS
# This is a linear regression, but may as well follow the same methods
# Silver newdat
newdat_silver <- data.frame(x = seq(min(silver_SAR$aggDF[,1]),
                                   max(silver_SAR$aggDF[,1]),
                                   length.out = 100))
# Predict for silverswords
newdat_silver$fit <- predict(silver_SAR$segObj, newdata = newdat_silver)

## TETRAGNATHA
# Spider newdat
newdat_spider <- data.frame(x = seq(min(spider_SAR$aggDF[,1]),
                                    max(spider_SAR$aggDF[,1]),
                                    length.out = 100))
# Predict for spiders
newdat_spider$fit <- predict(spider_SAR$linObj, newdata = newdat_spider)

## Silversword + Tetragnatha SAR
st_SAR <- ggplot(silver_SAR$aggDF, aes(x = x, y = y)) + 
  geom_point(color = "#D55E00") +
  geom_line(data = newdat_silver, aes(x = x, y = fit), color = "#D55E00", linewidth = 1) +
  # Spider data
  geom_point(data = spider_SAR$aggDF, aes(x = x, y = y), color = "#0072B2") +
  geom_line(data = newdat_spider, aes(x = x, y = fit), color = "#0072B2", linewidth = 1) +
  labs(x = expression(paste("Island Area (", "km"^"2", ")")),
       y = "Log(Number of Species)") +
  # Add custom tick labels for km^2
  scale_x_continuous(
    breaks = c(13.81551, 16.1181, 18.42068, 20.72327,
               23.02585, 25.32844, 27.63102),
    labels = expression(10^0, 10^1, 10^2, 10^3, 
                        10^4, 10^5, 10^6)) +
  theme_classic()

#### Plot all SARs in a column next to maps ####
# af_SAR = anole frog
# fs_SAR = finch scalesia
# st_SAR = silversword tetragnatha

# carib_plot, galap_plot, hawaii_plot = colored maps

# Save all of the legends to add to their own column
carib_leg <- get_legend(carib_plot + theme(legend.title = element_blank(),
                                           legend.text = element_text(size = 8)))
galap_leg <- get_legend(galap_plot + theme(legend.title = element_blank(),
                                           legend.text = element_text(size = 8)))
hawaii_leg <- get_legend(hawaii_plot + theme(legend.title = element_blank(),
                                             legend.text = element_text(size = 8)))

# Add the okina for Hawaii (\u02BB)
library(showtext)
# Maybe I should use a different font...
font_add_google("Noto Sans", "noto")
showtext_auto()

# Set the font theme outside of the grid so it applies to everything
theme_set(theme_minimal(base_family = "noto"))

cairo_pdf("testing_fig1_legs.pdf")
plot_grid(
  # Maps
  plot_grid(carib_plot + theme(legend.position = "none"),
            galap_plot + theme(legend.position = "none"),
            hawaii_plot + theme(legend.position = "none"),
            align = "v",
            ncol = 1,
            # Add labels to the left of maps (x = 0)
            # And at the top of each row (y = 1)
            labels = c("a) Caribbean", "b) Galápagos", "c) Hawai\u02BBi"),
            label_x = 0,
            label_y = 1),         
  # Legends
  plot_grid(carib_leg,
            galap_leg,
            hawaii_leg,
            align = "v",
            ncol = 1),
  # SARs
  plot_grid(af_SAR,
            fs_SAR,
            st_SAR,
            align = "v",
            ncol = 1),
  ncol = 3,
  rel_widths = c(0.9, 0.4, 1)
)
dev.off()

## What if the SARs were all scaled to have the same limits?
lim_af <- af_SAR + xlim(10,26) + ylim(0,5)
lim_fs <- fs_SAR + xlim(10,26) + ylim(0,5)
lim_st <- st_SAR + xlim(10,26) + ylim(0,5)

pdf("testing_fig1_lims.pdf")
plot_grid(
  plot_grid(carib_plot + theme(legend.position = "none"),
            galap_plot + theme(legend.position = "none"),
            hawaii_plot + theme(legend.position = "none"),
            align = "v",
            ncol = 1,
            # Add labels to the left of maps (x = 0)
            # And at the top of each row (y = 1)
            labels = c("a)", "b)", "c)"),
            label_x = 0,
            label_y = 1),         
  one_leg,
  plot_grid(lim_af,
            lim_fs,
            lim_st,
            align = "v",
            ncol = 1),
  ncol = 3,
  rel_widths = c(0.9, 0.3, 1)
)
dev.off()