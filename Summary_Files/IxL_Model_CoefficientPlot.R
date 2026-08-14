# Creating a coefficient plot for model-averaged coefficients

rich_coef_data <- read.csv("Summary_Files/richness_conf_df.csv")
pres_coef_data <- read.csv("Summary_Files/presence_conf_df.csv")

# Combine data for plotting
rich_coef_data$model <- "Richness"
pres_coef_data$model <- "Presence"
combined_coef <- rbind(rich_coef_data, pres_coef_data)

# Note whether each value is significant (TRUE) or not (FALSE) based on
#   whether the confidence interval includes zero
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

#### Create Pointrange Plot with Vertical Facet Wrap ####
labels <- c("area" = "Area",
            "dist_occ_islands" = "Dist. to Island with Occurence",
            "dist_mainland" = "Dist. to Mainland",
            "Nearest_Dist" = "Dist. to Nearest Island",
            "max_elev" = "Maximum Elevation",
            "mean_csi" = "Mean CSI",
            "n_habitat" = "Number of Habitats",
            "sd_csi" = "Standard Dev. CSI",
            "TRI" = "TRI")

base_plot <- ggplot(combined_coef, 
       aes(x = estimate, y = model, color = model)) +
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
  # Facet wrap by names and add proper labels
  facet_wrap(~ names,
             ncol = 1,
             labeller = as_labeller(labels)) +
  # Overall color of pointranges
  scale_color_manual(values = c("Richness" = "#E69F00",
                                "Presence" = "#56B4E9"),
                     breaks = c("Richness", "Presence")) +
  labs(x = "Coefficient Estimate with 95% CI", y = NULL, color = "Response") +
  theme_minimal() +
  # Remove the grid  
  theme(panel.grid = element_blank(),
        # Increase text size
        text = element_text(size = 14),
        # Remove the legend
        legend.position = "none",
        # Add a box around each facet
        panel.border = element_rect(color = "black"),
        # Remove the y axis text
        axis.text.y = element_blank())

# Maybe I should just restrict the width of the panels
#   and add the regular legend back to where it was
pdf("coefficientplot.pdf", width = 3, height = 8)
facet_plot <- base_plot + 
  # Limit the size of each facet to the range of the CIs
  scale_x_continuous(limits = c(-1.7, 2.6)) #+
  # Add the legend
  # theme(legend.position = "right")
facet_plot

dev.off()
