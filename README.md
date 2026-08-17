# Decoupled drivers of presence, richness, and speciation in island adaptive radiations
This GitHub repo contains R scripts and data necessary to replicate the analyses in our manuscript titled, "Decoupled drivers of presence, richness, and speciation in island adaptive radiations." 

## File Structure
* "Caribbean" directory - Contains files specific to Caribbean *Anolis* and *Eleutherodactylus*. Includes two subdirectories: "Data" and "Shapefile"
  - "Data" directory - Includes raw data and R objects used in determining species-area relationships, speciation-area relationships, and taxon-specific distance metrics.
  - "Shapefile" directory - Includes files needed to read the shapefile for the Caribbean islands.
* "Galapagos" directory - Contains files specific to Galapagos finches and *Scalesia*. Includes two subdirectories: "Data" and "Shapefile"
  - "Data" directory - Includes raw data and R objects used in determining species-area relationships, speciation-area relationships, and taxon-specific distance metrics.
  - "Shapefile" directory - Includes files needed to read the shapefile for the Galapagos islands.
* "habitat_diversity" directory - Contains files needed to estimate habitat heterogeneity for taxa of interest, along with IUCN shapefiles used to determine ranges (when applicable). 
  - CaribbeanAnoles directory - Contains a shapefile of *Anolis* ranges gathered via IUCN.
  - CaribbeanEleutherodactylus directory - Contains a shapefile of *Eleutherodactylus* ranges gathered via IUCN.
  - GalapagosFinches directory - Contains a shapefile of Galapagos finch ranges gathered via IUCN.
  - HawaiianAsteraceae directory - Contains a shapefile of Hawaiian *Asteraceae* ranges gathered via IUCN.
  - 1_crop_habitat_raster.R - Crops the worldwide habitat raster to the archipelagos of interest.
  - 2_habitat_diversity_lv2_island.R - Estimates habitat diversity for each lineage of interest for each island on which they are present.
  - 3_habitat_diversity_island_notaxa.R - Estimates habitat diversity for each island within each archipelago of interest.
* "Hawaiian" directory - Contains files specific to Hawaiian silverswords and *Tetragnatha*. Includes two subdirectories: "Data" and "Shapefile"
  - "Data" directory - Includes raw data and R objects used in determining species-area relationships, speciation-area relationships, and taxon-specific distance metrics.
  - "Shapefile" directory - Includes files needed to read the shapefile for the Hawaiian islands.
* "power" directory - Contains the following files used to run the power analysis that determines the probability of finding breakpoints in SARs and SpARs.
  - power.csv - A CSV containing the results of the SAR power analysis. Contains probabilities for finding a breakpoint with a positive second slope when archipelago-wide parameters are used.
  - power_lineages.csv - A CSV containing the results of the SAR power analysis. Contains probabilities for finding a breakpoint with a positive second slope when lineage-specific parameters are used.
  - power_null.csv - A CSV containing the results of a modified SAR power analysis that determines how often the null hypothesis is not rejected.
  - power_sar.R - R script that determines probabilities for finding a breakpoint with a positive second slope when archipelago-wide parameters are used.
  - power_sar_lineages.R - R script that determines probabilities for finding a breakpoint with a positive second slope when lineage-specific parameters are used.
  - power_sar_null.R - R script that includes a modified SAR power analysis that determines how often the null hypothesis is not rejected.
  - power_spar.csv - A CSV containing the results of the SpAR power analysis. Contains probabilities for finding a breakpoint with a positive second slope when archipelago-wide parameters are used.
  - power_spar.R - R script that determines probabilities for finding a breakpoint with a positive second slope in SpARs when archipelago-wide parameters are used.
  - power_spar_lineages.csv - A CSV containing the results of the SpAR power analysis. Contains probabilities for finding a breakpoint with a positive second slope when lineage-specific parameters are used.
  - power_spar_lineages.R - R script that determines probabilities for finding a breakpoint with a positive second slope in SpARs when lineage-specific parameters are used.
  - power_spar_null.csv - A CSV containing the results of a modified SpAR power analysis that determines how often the null hypothesis is not rejected.
  - power_spar_null.R - R script that includes a modified SpAR power analysis that determines how often the null hypothesis is not rejected.
* "Summary_Files" directory - Contains the following files used to generate data, R objects, and figures that represent the full dataset.
  - continents.rds – An R object containing spatial information related to the continents closest to the archipelagos of interest.
  - CSI.R - An R script that extracts Climate Shift Index values for all islands in the three archipelagos of interest
  - csi_past.tiff - From Herrando-Moraira et al. 2022. A GEOtiff that contains Climate Shift Index data, used with CSI.R. 
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

