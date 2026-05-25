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
