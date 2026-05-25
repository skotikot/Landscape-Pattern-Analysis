#' -----------------------------------------------------------------------------
#' Project: Landscape pattern analysis
#' Script: Automated Multi-Temporal Class and Landscape Metric Analysis
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------
#' Purpose:
#' Programmatically batch-loads multi-temporal rasters, applies 
#' reclassifications, performs modal focal window smoothing, standardizes 
#' metadata parameters, and calculates class- and landscape-level metrics 
#' via `landscapemetrics`.
#' -----------------------------------------------------------------------------

# =============================================================================
# 0. Load Required Libraries
# =============================================================================
library(dplyr)
library(tidyr)
library(stringr)
library(terra)             # Core spatial raster engine (replaces 'raster')
library(landscapemetrics)  # Categorical spatial pattern quantification

# =============================================================================
# 1. Configuration
# =============================================================================
DATA_DIR    <- "../Data/Processed"
INPUT_SHP  <- "../data/Raw"
OUTPUT_DIR  <- "../Data/Analysis_Outputs"

# Locate all multi-temporal scenario rasters matching expected prefix structures
raster_files <- list.files(
  path        = DATA_DIR, 
  pattern     = "^(CON|GR|GRPR|PR|all)_\\d{4}\\.tif$", 
  full.names  = TRUE
)


# Load files into an explicitly named list object
narok_rasters <- lapply(raster_files, terra::rast)
names(narok_rasters) <- tools::file_path_sans_ext(basename(raster_files))

# Ensure list structure is sorted consistently for index assignment matches
narok_rasters <- narok_rasters[order(names(narok_rasters))]

# =============================================================================
# 2. Reclassification & Neighborhood Smoothing (Focal Process)
# =============================================================================
# Reclassification Matrix: Re-maps categories into unified 2-class index limits
# (e.g., 1 = Forest, 2 = Rangeland, everything else is forced to NA)
rcl_mat <- matrix(c(
  -Inf,    0,   NA,  
  0,    1,    1,  
  1,    2,    2,  
  2,    3,    2,  
  3,  Inf,   NA
), ncol = 3, byrow = TRUE)

# Global lookup definitions for categorical mapping structures
cats3 <- data.frame(Value = 1:2, LandCover = c("Forest", "Rangeland"))

# Processes each raster array iteratively to clean boundary anomalies
processed_rasters <- lapply(names(narok_rasters), function(nm) {
  r <- narok_rasters[[nm]]
  
  # Condition: Apply 2-class reclassification only to CON layers and GR_1990
  if (str_detect(nm, "^CON_") || nm == "GR_1990") {
    r <- terra::classify(r, rcl_mat)
    levels(r) <- cats3
  }
  
  # Execute modal structural smoothing inside a 3x3 window matrix
  r_focal <- terra::focal(r, w = 3, fun = "modal", na.rm = TRUE)
  
  # Standardize spatial data attributes to maximize landscapemetrics compliance
  terra::NAflag(r_focal) <- 99
  names(r_focal)         <- nm
  
  return(r_focal)
})
names(processed_rasters) <- names(narok_rasters)

# Diagnostic layout confirmation check on a sample baseline layout
# landscapemetrics::check_landscape(processed_rasters[[1]])

# =============================================================================
# 3. Structural Indicator Computations (landscapemetrics)
# =============================================================================

# Class-Level Metrics: PLAND (Area Proportion), ED (Edge Density), CONTIG_MN (Contiguity Mean)
metrics_class <- landscapemetrics::calculate_lsm(
  landscape     = processed_rasters, 
  what          = c("lsm_c_pland", "lsm_c_ed", "lsm_c_contig_mn"), 
  neighbourhood = 8, 
  directions    = 8, 
  progress      = TRUE
)

# Landscape-Level Metrics: ED (Edge Density), CONTAG (Contagion), PRD (Patch Richness Density)
metrics_landscape <- landscapemetrics::calculate_lsm(
  landscape     = processed_rasters, 
  what          = c("lsm_l_ed", "lsm_l_contag", "lsm_l_shdi"), 
  neighbourhood = 8, 
  directions    = 8, 
  progress      = TRUE
)

# =============================================================================
# 4. Tidy Data Transformation & Policy Relabeling
# =============================================================================

# Create dynamic lookup index mapping to substitute standard integers with layout labels
layer_names_vector <- setNames(names(processed_rasters), seq_along(processed_rasters))

# Policy name replacements mapping data abbreviations to final publication terms
policy_lookup <- c(
  "CON"   = "Conservancy (GR-PR-CON)",
  "GR"    = "Group ranch (GR)",
  "PR"    = "Private (PR)",
  "GRPR"  = "Private (GR-PR)",  # Normalized from spatial 'GR_PR' string split
  "all"   = "All policy areas"
)

# Class type lookup translation string matrix
class_lookup <- c(
  "1" = "Forest",
  "2" = "Rangeland",
  "3" = "Cropland"
)

# Reference land area tracking allocations (km²) mapped across policy configurations
study_sites <- sf::st_read(file.path(INPUT_SHP, "StudySites.shp"))

# Project vector layer into exact matching spatial coordinate registry
study_sites_trans <- sf::st_transform(study_sites, crs = terra::crs(processed_rasters[[1]]))

# . Calculate area (returns a 'units' object, usually in square meters)
study_sites_trans$area_m2 <- st_area(study_sites_trans)

# . Convert to numeric square kilometers
study_sites_trans$area_km2 <- as.numeric(study_sites_trans$area_m2) / 1000000

policy_zone_rename <- c(
  "PR"              = "Private (PR)",
  "CON"   = "Conservancy (GR-PR-CON)",
  "GR"          = "Group ranch (GR)",
  "GR_PR"           = "Private (GR-PR)",
  "All policy areas"          = "All policy Area"
)

# . Apply it to your dataset
study_sites_trans_cleaned <- study_sites_trans %>%
  mutate(Policy = policy_zone_rename[Policy])

# . Group by your policy class, sum the areas, and pull into a named vector
area_lookup <- study_sites_trans_cleaned %>%
  st_drop_geometry() %>% 
  group_by(Policy) %>% 
  summarise(total_area = round(sum(area_km2), 0)) %>% 
  { setNames(.$total_area, .$Policy) }

# . Append the global total to match your exact format
all_policy_total <- round(sum(study_sites_trans_cleaned$area_km2), 0)
area_lookup["All policy areas"] <- all_policy_total

# area_lookup <- c(
#   "Private (PR)"              = 231,
#   "Conservancy (GR-PR-CON)"   = 375,
#   "Group ranch (GR)"          = 714,
#   "Private (GR-PR)"           = 544,
#   "All policy areas"          = 1861
# )

# Refactor Class Data Outputs
narok_class_clean <- metrics_class %>%
  mutate(
    layer = recode(as.character(layer), !!!layer_names_vector),
    class = recode(as.character(class), !!!class_lookup)
  ) %>%
  separate(col = layer, into = c("policy", "Year"), sep = "_", fill = "right") %>%
  mutate(
    policy = recode(policy, !!!policy_lookup),
    across(c(policy, Year, class, metric), as.factor)
  )

# Refactor Global Landscape Data Outputs
narok_landscape_clean <- metrics_landscape %>%
  mutate(layer = recode(as.character(layer), !!!layer_names_vector)) %>%
  separate(col = layer, into = c("policy", "Year"), sep = "_", fill = "right") %>%
  mutate(
    policy = recode(policy, !!!policy_lookup),
    lArea  = recode(as.character(policy), !!!area_lookup),
    # If using Shannon's Diversity Index (shdi), standardize metric value by polygon area boundaries
    value  = if_else(metric == "shdi", value / lArea, value),
    across(c(policy, Year, metric), as.factor)
  )

# =============================================================================
# 5. Archive and Export Summary Tables
# =============================================================================
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

readr::write_csv(narok_class_clean, file.path(OUTPUT_DIR, "processed_ClassMetrics.csv"))
readr::write_csv(narok_landscape_clean, file.path(OUTPUT_DIR, "processed_LandscapeMetrics.csv"))





