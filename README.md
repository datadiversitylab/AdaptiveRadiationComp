# Repo for Comparing Global Adaptive Radiations Through Species- and Speciation-Area Relationships

# WORK IN PROGRESS

## File Structure
* "Summary_Files" directory - Contains the following files used to generate data, R objects, and figures that represent the full dataset.
  - continents.rds – An R object containing spatial information related to the continents closest to the archipelagos of interest.
  - estimate_MS_AR.R – Contains an edited version of the “estimate_MS” function from the ssarp R package that allows for diversification rate estimation with different epsilon values.
  - Figure2.R – Generates Figure 2, which visualizes the islands on which species in each taxon occur, along with their associated species-area relationships.
  - Find_Closest_Mainland.R – Uses global polygons from Natural Earth to determine the distance between each island in the dataset and the closest mainland.
  - FixNames.R – Includes functions that add appropriate accents back to island names that have been corrupted due to character encoding problems.
  - Infer_SARs.R - Infers species-area relationships (SARs) for taxa of interest and generates .rds files of SAR objects that are used in Figure2.R.
  - island_traits.csv – Values for island-specific predictor variables, including the total number of habitats on each island (not filtered by taxon).
  - IxL.csv – Values for predictor variables for each island, specifically related to each taxon (IxL = “island by lineage”).
  - IxL_Model_CoefficientPlot.R – Generates Figure 4, which visualizes model-averaged coefficients and their 95% confidence intervals.
  - IxL_ModelSelection.R – Generates GLMMs to determine drivers of richness and presence for island-endemic adaptive radiations.
  - IxL_Variables_Pointrange.R – Generates Figure 1, which visualizes the means and standard error around island-level variables used in GLMMs. Also, generates Figure S2, which visualizes the range of island areas for each archipelago.
  - presence_conf_df.csv – Dataset containing coefficient point estimates and confidence intervals for models with presence as the response used in IxL_Model_CoefficientPlot.R
  - richness_conf_df.csv – Dataset containing coefficient point estimates and confidence intervals for models with richness as the response used in IxL_Model_CoefficientPlot.R
  - SpAR_Figures.R – Generates Figures 3, S3, and S4 depending on what epsilon value the user chooses.
  - Varied_Epsilon_SpARs.R – Generates speciation-area relationships for all taxa using three different values for epsilon: 0, 0.5, and 0.9.


## File Structure (Old)
 * "Caribbean" directory: includes a script and associated data (in "Caribbean/Data") for creating SARs and SpARs for *Anolis* lizards, *Eleutherodactylus* frogs, and *Tropidophis* snakes.
 * "Galapagos" directory: includes a script and associated data (in "Galapagos/Data") for creating SARs and SpARs for *Microlophus* lizards, Galápagos finches, and *Scalesia* plants. This directory also includes a "Shapefile" subdirectory and the "Galapagos_Finch_Isolation.R" R script for calculating island isolation metrics.
 * "Hawaiian" directory: includes a script and associated data (in "Hawaiian/Data") for creating SARs and SpARs for *Pritchardia* palms, Hawaiian Silverswords, and *Tetragnatha* spiders.
 * "Slope_Comparisons" directory: includes scripts and data for my original jackknife comparison of SAR and SpAR slopes
