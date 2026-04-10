# Creating a coefficient plot for model-averaged coefficients

rich_coef_data <- read.csv("Summary_Files/richness_conf_df.csv")
pres_coef_data <- read.csv("Summary_Files/presence_conf_df.csv")

# Combine data for plotting
rich_coef_data$model <- "Richness"
pres_coef_data$model <- "Presence"
combined_coef <- rbind(rich_coef_data, pres_coef_data)

# Create a column that adds an asterisk to significant pointranges
# combined_coef$significant <- rep("*", length(combined_coef$names))
# for(i in c(1:length(combined_coef$names))){
#   if(combined_coef$lower[i] < 0 && combined_coef$upper[i] > 0){
#     combined_coef$significant[i] <- ""
#   }
# }

# Instead of an asterisk, just use TRUE/FALSE
combined_coef$significant <- rep(TRUE, length(combined_coef$names))
for(i in c(1:length(combined_coef$names))){
  if(combined_coef$lower[i] < 0 && combined_coef$upper[i] > 0){
    combined_coef$significant[i] <- FALSE
  }
}

# Make sure that the order is alphabetical
combined_coef$names <- factor(combined_coef$names, 
                              levels = c("area", "dist_occ_islands", 
                                         "dist_mainland", "Nearest_Dist",
                                         "max_elev", "mean_csi", "n_habitat",
                                         "sd_csi", "TRI"))

# Use position_dodge() within geom_pointrange() to stack
ggplot(combined_coef, 
       aes(x = estimate, y = names, color = model, group = model)) +
  geom_vline(xintercept = 0, color = "black") +
  # Specify that the fill is based on significance
  geom_pointrange(aes(xmin = lower, xmax = upper, 
                      fill = interaction(model, significant)),
                  shape = 21,
                  position = position_dodge(width = 0.5)) +
  # When significant = TRUE, fill points with correct color
  # When significant = FALSE, do not fill points
  # "guide = "none"" removes the legend for fills specifically
  scale_fill_manual(values = c("Richness.TRUE" = "#E69F00",
                               "Presence.TRUE" = "#56B4E9",
                               "Richness.FALSE" = NA,
                               "Presence.FALSE" = NA),
                    guide = "none") +
  # Specify names on y-axis
  # They're reversed for some reason, so reverse back??
  scale_y_discrete(limits = rev,
                    labels = c(area = "Area",
                               dist_occ_islands = "Dist. to Island with Occurence",
                               dist_mainland = "Dist. to Mainland",
                               Nearest_Dist = "Dist. to Nearest Island",
                               max_elev = "Maximum Elevation",
                               mean_csi = "Mean CSI",
                               n_habitat = "Number of Habitats",
                               sd_csi = "Standard Dev. CSI",
                               TRI = "TRI")) +
  # Overall color of pointranges
  scale_color_manual(values = c("Richness" = "#E69F00",
                                "Presence" = "#56B4E9"),
                     breaks = c("Richness", "Presence")) +
  #labs(x = "Coefficient Estimate with 95% Confidence Interval", y = NULL, color = "Response Variable") +
  labs(x = NULL, y = NULL, color = "Response Variable") +
  theme_minimal() +
  # Remove the grid
  theme(panel.grid = element_blank(),
        text = element_text(size = 14),
        legend.position = c(0.8, 0.5))
