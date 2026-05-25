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

Core Categorical Data ClassesThe processed raster layers represent a standardized 3-class land cover model optimized for agropastoral landscape analysis:Class 1 (Forest): Native evergreen canopy, riverine thickets, and protected montane woodlots.Class 2 (Rangeland): Savannah grasslands, open scrublands, and communal woody grazing matrix.Class 3 (Cropland): Smallholder active agriculture, mechanized farming, and commercial cultivation.Pipeline Execution Workflow1. Land Cover Ingestion and Extent MaskingScript: LULC_DataPrep.RFunction: Ingests multi-temporal historical LULC grids (1974, 1990, 2000, 2010, 2018). It normalizes the schemas into uniform 3-class profiles, establishes coordinate reference system (CRS) tracking via EPSG:32737 (UTM Zone 37S), maps color tables, and clips boundaries down to discrete spatial management units defined by StudySites.shp.2. Categorical Spatial Pattern QuantificationScript: calc_LandscapeMetrics.RFunction: Leverages the landscapemetrics engine to batch-process the processed time-series grids. It extracts fragmentation and composition statistics across sub-regional polygon zones—calculating class-level metrics (e.g., Percentage of Landscape % PLAND, Edge Density ED) and global landscape configurations (e.g., Shannon's Diversity Index SHDI).3. Multi-Temporal Transition Intensity AnalysisScript: calc_plot_IntensityAnalysis.RFunction: Implements a robust S4 class parsing workflow using the OpenLand framework. It computes contingency tables and processes categorical change pathways into a standardized database. This script models three primary pathways:Cropland Gain: Encroachment into native forest and rangeland matrices.Forest Loss: Conversion dynamics shifting forest pixels to agriculture or open pasture.Rangeland Loss: Depletion of communal grazing envelopes.It compiles these paths into a composite multi-panel plot tracking observed change intensities against baseline uniform intensities.4. Publication Graph CompositionScript: plot_LandscapeMetrics.RFunction: Consolidates final comma-separated values (.csv) summaries to render polished, color-blind friendly visualizations. It overlays historical timelines, maps multi-tenure paths, standardizes scales relative to polygon bounding areas, and writes high-resolution figures optimized with LZW compression for publisher submission.Software & Computational DependenciesThe codebase is written and verified using R (Version $\ge$ 4.0.0). The following external libraries are required:R# Execute within R console to install the necessary repository environment
install.packages(c(
  "sf",               # Simple features vector handling
  "terra",            # High-performance grid raster processing 
  "raster",           # Required safely for legacy OpenLand compatibility
  "landscapemetrics", # Categorical spatial pattern quantification
  "OpenLand",         # Intensity analysis workflows
  "tidyverse",        # Consolidated data wrangling ecosystem (dplyr, tidyr, stringr)
  "cowplot",          # Multi-panel figure arrangements
  "grid"              # Core visualization geometry engine
))
Funding & AcknowledgmentsThis work was supported by the National Science Foundation (NSF) BCS Award No. 2149244. We thank our collaborating partner institutions, including The Pennsylvania State University, University of Connecticut, University of British Columbia, and Maasai Mara University.LicenseThis project is licensed under the MIT License - see the LICENSE file for details.
