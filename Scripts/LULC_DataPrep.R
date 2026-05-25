#' -----------------------------------------------------------------------------
#' Project: Landscape Pattern analysis (pre-processing)
#' Script: LULC Data Preparation and Extent Masking (3 Classes)
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------
#' Purpose: 
#' Ingests multi-temporal, Landsat-derived Land Use / Land Cover (LULC) data layers,
#' standardizes classification schemas into clean 3-class profiles, applies uniform
#' visualization attributes (factors and color tables), aligns spatial coordinate 
#' tracking (CRS), and crops data vectors down to localized policy management units.
#' -----------------------------------------------------------------------------

# =============================================================================
# 0. Load Required Libraries
# =============================================================================
library(sf)             # Modern simple features vector handling
library(terra)          # Optimized grid raster computing engine
library(dplyr)          # Tidy framework data wrangling

# =============================================================================
# 1. Parameterize File Paths & Working Paths
# =============================================================================
INPUT_DIR  <- "../data/Raw"
OUTPUT_DIR <- "../data/Processed"

# =============================================================================
# 2. Ingest Historical & Target LULC Rasters
# =============================================================================

sr <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Load historical spatial baselines via terra engine
lulc1974 <- project(terra::rast(file.path(INPUT_DIR, "lulc1974.tif")), sr)
lulc1990 <- project(terra::rast(file.path(INPUT_DIR, "lulc1990.tif")), sr)
lulc2000 <- project(terra::rast(file.path(INPUT_DIR, "lulc2000.tif")), sr)
lulc2010 <- project(terra::rast(file.path(INPUT_DIR, "lulc2010.tif")), sr)
lulc2018 <- project(terra::rast(file.path(INPUT_DIR, "lulc2018.tif")), sr)

#Change the extent of the 1974 layer to match the other ones to allow for stacking
stack1 <- c(lulc1990, lulc2000, lulc2010, lulc2018)
stack2 <- terra::ifel(stack1 > 0, 1, NA)
stack3 <- terra::ifel(!is.na(stack2), 1, 0)
stackSum = terra::app(stack3, fun=sum)
stackSum2 <- terra::ifel(stackSum == 4, 1, NA)# my new extent raster

lulc1974r <- terra::resample(lulc1974, stackSum2, method = 'near')
lulc1974m <- terra::mask(terra::crop(lulc1974r, stackSum2), stackSum2)

s <- c(lulc1974m, lulc1990,lulc2000,lulc2010,lulc2018)

# =============================================================================
# 3. Categorical Adjustments & Color Palette Formats
# =============================================================================

m <- c(-Inf,0, NA,  0, 1, 1,  1, 2, 2,  2,4,3, 4, Inf, NA)#Rangelands
rclmat <- matrix(m, ncol=3, byrow=TRUE)
s_rec2 <- classify(s[[2:5]], rclmat)

#------------------------------------------------------------------------------------
f1 <- s[[1]]
m2 <- c(-Inf,0, NA,  0, 1, 1,  1, 3, 2,  3,4,3,  4, Inf, NA)#rangeland
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
s_rec1 <- classify(f1, rclmat2)

srec <- c(s_rec1, s_rec2)

# =============================================================================

# Establish explicit 3-class data mapping frames
cls3  <- c("Forest", "Rangeland", "Cropland")
cats3 <- data.frame(Value = 1:3, LandCover = cls3)

# Establish matching publication-grade color hex matrix tracking
coltb <- data.frame(value = 1:3, col = c("green4", "khaki1", "rosybrown"))

# Loop through all internal layers to inject attributes cleanly without repetition
for (i in 1:terra::nlyr(srec)) {
  levels(srec[[i]]) <- cats3
  terra::coltab(srec[[i]]) <- coltb
}

# =============================================================================
# 4. Ingest Vectors, Align Coordinates & Execute Spatial Cropping
# =============================================================================

# Read administrative policy boundaries
study_sites <- sf::st_read(file.path(INPUT_DIR, "StudySites.shp"))

# Project vector layer into exact matching spatial coordinate registry
study_sites_trans <- sf::st_transform(study_sites, crs = terra::crs(srec))
study_sites_trans <- study_sites_trans %>%
  mutate(Policy = case_when(
    Policy == "GR_PR" ~ "GRPR",
    TRUE ~ as.character(Policy) # Keeps all other column values exactly the same
  ))

# Extract policy descriptive markers to vector arrays
policy_names <- study_sites_trans$Policy

# =============================================================================
# 5. Data Export & File Serialization
# =============================================================================
# Enforce integer structure constraints on cell values to minimize final file sizes
s_export <- srec
s_export[] <- as.integer(s_export[])

for (i in 1:length(policy_names)){
  grp1 <- study_sites_trans %>% 
    dplyr::filter(Policy==policy_names[i])
  crp1 <- terra::crop(s_export, grp1)
  crp2 <- terra::mask(crp1, grp1)
  
  nms2 <- paste0(OUTPUT_DIR, "/",policy_names[i],"_",c(1974,1990,2000,2010,2018),".tif")
  terra::writeRaster(crp2, nms2, datatype="INT4U", overwrite=T)
  
}

#mask for the whole landscape - all policies
nms2 <- paste0(OUTPUT_DIR, "/","all_",c(1974,1990,2000,2010,2018),".tif")

crp1 <- terra::crop(srec, study_sites_trans)
crp2 <- terra::mask(crp1, study_sites_trans)
crp2[]=as.integer(crp2[])
terra::writeRaster(crp2, nms2, datatype="INT4U", overwrite=T)


