# Creating a faceted boxplot visualization to illustrate
#  the spread of island-specific data
library(tidyverse)
library(ggplot2)
library(reshape2)

# Read dataset
dat <- read.csv("Summary_Files/IxL_distocc_2_17.csv")

# If an island has an NA for dist_occ_islands,
#  fill in the value from nearest_occ
for(i in c(1:length(dat$name))){
  if(is.na(dat$dist_occ_islands[i])){
    dat$dist_occ_islands[i] <- dat$nearest_occ[i]
  }
}

# Filter to just island-specific variables, and only ones that aren't correlated
dat_island <- dat %>% dplyr::select(name, TRI, max_elev, mean_csi, sd_csi, area,
                             dist_mainland, archipelago)
# Make sure each row is unique
dat_island <- distinct(dat_island)

# For each island-specific variable, create a boxplot to show the spread

# Got idea for melting from: https://stackoverflow.com/a/11346964
m1 <- melt(dat_island)

# Rearrange factor levels to be alphabetical?
m1$variable <- factor(m1$variable, levels = c("dist_mainland", "area",
                                              "max_elev", "mean_csi",
                                              "sd_csi", "TRI"))

ggplot(m1, aes(x = archipelago, y = value)) +
  geom_boxplot(aes(fill = archipelago)) +
  scale_fill_manual(values = c("carib" = "#E69F00", "galap" = "#56B4E9", "hawaii" = "#CC79A7")) +
  # Add free_y scale so they can vary
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  # Remove outer axis labels
  labs(x = "", y = "")

##### Instead of Boxplot, plot means and add standard error bars #####
ggplot(m1, aes(x = archipelago, y = value)) +
  # Use stat_summary to quickly calculate mean and standard error
  stat_summary(fun = "mean", geom = "point", size = 2) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  # Add outer axis labels
  xlab("Archipelago") +
  ylab("Value") +
  # Add facet wrap by variable with a free y-axis range and
  #  specific facet labels
  facet_wrap(~ variable, 
             axes = "all_x",
             scales = "free_y",
             nrow = 1,
             labeller = as_labeller(c(
               dist_mainland = "Distance to Mainland",
               area = "Island Area",
               max_elev = "Maximum Elevation",
               mean_csi = "Mean CSI",
               sd_csi = "Standard Dev. CSI",
               TRI = "TRI"
               ))) +
  # Change per-plot x-axis labels
  scale_x_discrete(labels = c("C", "G", "H")) +
  theme_minimal() +
  # Remove gridlines, add axes in gray
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "gray70", linewidth = 0.5))
        # Angle tick text
        #axis.text.x = element_text(angle = 90, hjust = 1))
  # theme(panel.grid = element_blank(),
  #       strip.background = element_rect(fill = "grey95", color = "grey95"),
  #       strip.text = element_text(color = "black", face = "bold"))
