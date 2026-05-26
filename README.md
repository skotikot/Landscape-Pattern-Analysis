# Historical Land Policies & Landscape Patterns in Agropastoral Systems (Narok County, Kenya)

## Repository Overview

This repository contains the complete reproducible R analytical pipeline, data preparation workflows, and visualization scripts associated with the following publication:

> **Kotikot, S. M., Smithwick, E. A. H., Gergel, S., Nankaya, J., Abila, R., & Mabwoga, S. (2025).** *Historical land policies influence contemporary landscape patterns in agropastoral landscapes.* **Landscape Ecology**, 40(163). [https://doi.org/10.1007/s10980-025-02178-x](https://doi.org/10.1007/s10980-025-02178-x)

### Project Context & Objectives
This project quantifies the enduring legacy effects of four distinct historical land policy trajectories over more than four decades (1974–2018) across Narok County, southern Kenya. 

By standardizing multi-temporal Landsat-derived Land Use / Land Cover (LULC) classifications into uniform management zones, this pipeline computes categorical landscape metrics and conducts transition intensity analysis. This allows for evaluation of how localized tenure regimes ranging from individual privatization to community group ranches and conservancies influence habitat fragmentation, forest loss, and cropland encroachment.

---

## Data Architecture & Directory Structure

To execute these scripts without path modifications, set up your local project root directories relative to the script paths as follows:

```text
├── Code/
│   ├── LULC_DataPrep.R                # Data standardization and masking
│   ├── calc_LandscapeMetrics.R        # Batch metric calculations via landscapemetrics
│   ├── calc_plot_IntensityAnalysis.R  # Multi-temporal change intensity analysis and visualization
│   └── plot_LandscapeMetrics.R        # Metrics visualization
├── Data/
    ├── Raw/                           # Source GIS layers (Landsat grids, StudySites.shp)
    ├── Processed/                     # Harmonized 3-class rasters (.tif)
    └── Analysis_Outputs/              # Quantified landscape summary tables (.csv)
```

Download data from [Zenodo][10.5281/zenodo.20384313] and extract into the Data folder. 

## Core Categorical Data Classes

The processed raster layers represent a standardized 3-class land cover model optimized for agropastoral landscape analysis:

- **Class 1 (Forest):** Native evergreen canopy, riverine thickets, and protected montane woodlots.  
- **Class 2 (Rangeland):** Savannah grasslands, open scrublands, and communal woody grazing matrix.  
- **Class 3 (Cropland):** Smallholder active agriculture, mechanized farming, and commercial cultivation.  

---

## Pipeline Execution Workflow

### 1. Land Cover Ingestion and Extent Masking

**Script:** `LULC_DataPrep.R`  

**Function:**  
Ingests multi-temporal historical LULC grids (1974, 1990, 2000, 2010, 2018).  
It normalizes schemas into uniform 3-class profiles, establishes coordinate reference system (CRS) tracking via **EPSG:32737 (UTM Zone 37S)**, maps color tables, and clips boundaries to spatial management units defined by `StudySites.shp`.

---

### 2. Categorical Spatial Pattern Quantification

**Script:** `calc_LandscapeMetrics.R`  

**Function:**  
Leverages the **landscapemetrics** engine to batch-process time-series grids.  
Extracts fragmentation and composition statistics across sub-regional polygon zones, including:

- Class-level metrics: Percentage of Landscape (**PLAND**), Edge Density (**ED**)  
- Landscape-level metrics: Shannon's Diversity Index (**SHDI**), , Edge Density (**ED**) 

---

### 3. Multi-Temporal Transition Intensity Analysis

**Script:** `calc_plot_IntensityAnalysis.R`  

**Function:**  
Implements an S4 class parsing workflow using the **OpenLand** framework.  
Computes contingency tables and processes categorical change pathways into a standardized database.

**Primary pathways modeled:**

- **Cropland Gain:** Encroachment into forest and rangeland  
- **Forest Loss:** Conversion to agriculture or rangeland
- **Rangeland Loss:** Conversion to agriculture or transition to forest 

Outputs a composite multi-panel visualization comparing observed vs. uniform intensity baselines.

---

### 4. Visualization

**Script:** `plot_LandscapeMetrics.R`  

**Function:**  
Consolidates `.csv` summaries to generate visualization figures:

---

## Software & Computational Dependencies

The codebase is written and verified using **R (Version ≥ 4.0.0)**.

```{r}
# Execute within R console to install required packages
install.packages(c(
  "sf",               # Simple features vector handling
  "terra",            # High-performance raster processing 
  "raster",           # Legacy compatibility (OpenLand)
  "landscapemetrics", # Spatial pattern metrics
  "OpenLand",         # Intensity analysis workflows
  "tidyverse",        # Data wrangling ecosystem
  "cowplot",          # Multi-panel figures
  "grid"              # Visualization engine
))
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
