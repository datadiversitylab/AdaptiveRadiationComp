# Creating a faceted boxplot visualization to illustrate
#  the spread of island-specific data
library(tidyverse)
library(ggplot2)
library(reshape2)

# Read dataset
dat <- read.csv("Summary_Files/IxL.csv")

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

##### Plot means and add standard error bars #####
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

##### Plot just the range of the areas #####
# Filter m1 to only include area
area_dat <- m1[which(m1$variable == "area"), ]

# Convert to km^2
area_dat$value <- area_dat$value/1e+6

# Add the okina for Hawaii (\u02BB)
library(showtext)
# Maybe I should use a different font...
font_add_google("Noto Sans", "noto")
showtext_auto()

cairo_pdf("testing_area_pointranges.pdf")
ggplot(area_dat, aes(x = archipelago, y = value)) +
  # Change to log scale
  scale_y_log10() +
  # Use stat_summary to quickly calculate mean, min, and max
  stat_summary(fun = mean, fun.min = min, fun.max = max, geom = "pointrange") +
  # Add dotted horizontal line at y = 986 km^2 (the smallest breakpoint - Scalesia)
  geom_hline(yintercept = 986, linetype = "dotted") +
  # Add title and outer axis labels
  labs(title = "Range of Island Areas") +
  xlab("Archipelago") +
  ylab(expression("Area " * "(km"^2 * ")")) +
  # Change per-plot x-axis labels
  scale_x_discrete(labels = c("Caribbean", "Galápagos", "Hawai\u02BBi")) +
  theme_minimal() +
  # Remove gridlines, add axes in gray, change font family and size
  theme(axis.text.x = element_text(family = "noto"),
        axis.text.y = element_text(family = "noto"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray70", linewidth = 0.5),
        text = element_text(size = 20))
dev.off()
