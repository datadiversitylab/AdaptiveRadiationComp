# Creating a coefficient plot for model-averaged coefficients

rich_coef_data <- read.csv("Summary_Files/richness_conf_df.csv")
pres_coef_data <- read.csv("Summary_Files/presence_conf_df.csv")

# Combine data for plotting
rich_coef_data$model <- "Richness"
pres_coef_data$model <- "Presence"
combined_coef <- rbind(rich_coef_data, pres_coef_data)

# Create a column that adds an asterisk to significant pointranges
combined_coef$significant <- rep("*", length(combined_coef$names))
for(i in c(1:length(combined_coef$names))){
  if(combined_coef$lower[i] < 0 && combined_coef$upper[i] > 0){
    combined_coef$significant[i] <- ""
  }
}

# Use position_dodge() within geom_pointrange() to stack
ggplot(combined_coef, 
       aes(x = estimate, y = names, color = model)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  position = position_dodge(width = 0.5)) +
  # Add asterisks for significance
  geom_text(aes(label = significant, size = 4, color = "black"),
            position = position_dodge(width = 0.5),
            hjust = -0.8,
            show.legend = FALSE) +
  # Specify names on y-axis
  scale_y_discrete(labels = c("Area", "Dist. to Mainland",
                              "Dist. to Island with Occurence",
                              "Maximum Elevation",
                              "Mean CSI",
                              "Number of Habitats",
                              "Dist. to Nearest Island",
                              "Standard Dev. CSI",
                              "TRI")) +
  scale_color_manual(values = c("Richness" = "#E69F00",
                                "Presence" = "#56B4E9")) +
  labs(x = "Coefficient Estimate with 95% Confidence Interval", y = NULL, color = "Response Variable") +
  theme_minimal() +
  # Remove the grid
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 14))
