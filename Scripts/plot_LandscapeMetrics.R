#' -----------------------------------------------------------------------------
#' Project: Landscape pattern change analysis
#' Script: Multi-Scale Structural and Compositional Landscape Visualization
#' Author: skotikot
#' Date: May 2026
#' -----------------------------------------------------------------------------
#' Purpose:
#' Ingests finalized tabular summaries of spatial metrics to compile
#' color-blind friendly figures:
#' 
#'   1. Landscape-Level Metrics: Visualizes temporal trajectories for Edge Density (ED) 
#'      and Shannon's Diversity Index (SHDI), overlaying discrete asterisks to mark 
#'      historical status quo baselines and key policy transition periods (1974, 2000, 2010).
#'      
#'   2. Class-Level Metrics & Proportions: Constructs a faceted response matrix tracking 
#'      fragmentation indices and percentage cover trends (% PLAND) broken down by 
#'      individual land use/land cover classes (Cropland, Forest, and Rangeland) 
#'      across contrasting land tenure regimes.
#' 
#' Inputs:
#'   - `../Data/Analysis_Outputs/processed_LandscapeMetrics.csv`
#'   - `../Data/Analysis_Outputs/processed_ClassMetrics.csv`
#' 
#' Outputs saved to `../Data/Figures/`:
#'   - `Fig_LandscapeMetrics.tiff` (Horizontal free-scaled path plot)
#'   - `Fig_ClassMetrics_ComparativeGrid.tiff` (2x3 Facet grid matrix)
#'   - `Fig_LULC_ClassProportions.tiff` (Stacked compositional % cover chart)

# =============================================================================
# 0. Load Required Libraries
# =============================================================================
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# 1. Environment Configurations & Data Ingestion
# =============================================================================
OUTPUT_DIR <- "../Data/Analysis_Outputs"
EXPORT_DIR <- "../Data/Figures"

# Read in finalized processed tabular datasets
narok_landscape <- read_csv(file.path(OUTPUT_DIR, "processed_LandscapeMetrics.csv"), show_col_types = FALSE)
narok_class <- read_csv(file.path(OUTPUT_DIR, "processed_ClassMetrics.csv"), show_col_types = FALSE)

##############################################################################################################
###########plot LANDSCAPE LEVEL METRICS
##############################################################################################################

# Clean dataset names & remove indices not targeted for this visualization canvas
data_in <- narok_landscape %>% 
  dplyr::filter(metric != "contag")

# =============================================================================
# 2. Plot Parameter Configurations & Label Mappings
# =============================================================================

# Publication-grade multi-panel facet titles
metric_labs <- c(
  "ed"   = "a) Edge density", 
  "shdi" = "b) Shannon's diversity index"
)

# Color-blind friendly qualitative color palette mapping scenario vectors
scenario_colors <- c(
  "Conservancy (GR-PR-CON)" = "#009E73", # Green
  "Group ranch (GR)"        = "#56B4E9", # Sky Blue
  "Private (PR)"            = "#E69F00", # Orange
  "Private (GR-PR)"         = "#0072B2", # Dark Blue
  "All policy areas"        = "#D55E00"  # Vermillion/Red
)


# Isolate historical milestone rows to act as explicit point overlays 
# (Filters to isolate the earliest timestamps or targeted status quo values)
baseline_highlights1 <- data_in %>%
  dplyr::filter(Year %in% c(1974))

baseline_highlights2 <- data_in %>%
  dplyr::filter(policy %in% c("Private (GR-PR)"  , "Conservancy (GR-PR-CON)") & Year == 2000)

baseline_highlights3 <- data_in %>%
  dplyr::filter(policy %in% c("Conservancy (GR-PR-CON)") & Year == 2010)

baseline_highlights <- bind_rows(baseline_highlights1, baseline_highlights2, baseline_highlights3)

baseline_highlights$ystar <- NA
baseline_highlights$yend <- NA
baseline_highlights <- within(baseline_highlights, {
  f <- metric == 'shdi' #prd
  ystar[f] <- value[f] - 0.05
  yend[f] <- value[f] + 0.05
  
  g <- metric == 'ed'
  ystar[g] <- value[g] - 12
  yend[g] <- value[g] + 12
  
  h <- metric == 'contag'
  ystar[h] <- value[h] - 3
  yend[h] <- value[h] + 3
  
})

# =============================================================================
# 3. Data Visualization Generation (ggplot2)
# =============================================================================

comparison_plot <- ggplot(data_in, aes(x = Year, y = value, color = policy)) +
  
  # Layer 1: Global Trajectory Lines grouped by administrative policy
  geom_line(aes(group = policy), size = 1.25, alpha = 0.85) +
  
  # Layer 2: Core Data Points mapped to track intersection vertices
  geom_point(size = 2.5, show.legend = FALSE) +
  
  # Layer 3: Overlay high-visibility Asterisks (Shape 8) tracking targeted baselines
  geom_point(
    data        = baseline_highlights, 
    aes(x = Year, y = value), 
    color       = "black", 
    size        = 6, 
    shape       = 8,
    show.legend = FALSE
  ) +
  
  # Set custom thematic color vectors
  scale_color_manual(values = scenario_colors) +
  
  # Split canvas into distinct columns with free, independent scale axes
  facet_wrap(
    ~metric, 
    scales   = "free_y", 
    ncol     = 2, 
    labeller = as_labeller(metric_labs)
  ) +
  
  # Presentation Label Formatting
  labs(
    y     = "Value",
    color = ""
  ) +
  
  # Apply minimalist theme aesthetics for clean academic rendering
  theme_minimal(base_size = 14) +
  theme(
    # Format facet header block surfaces
    strip.background = element_blank(),
    strip.text       = element_text(size = 14, color = "black", face = "bold", hjust = 0),
    
    # Text, Tick, and Axis styling configurations
    text             = element_text(family = "Cambria"),
    axis.text.x      = element_text(color = "black", size = 12),
    axis.text.y      = element_text(color = "black", size = 12),
    axis.title.x     = element_blank(),
    axis.title.y      = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    
    # Grid Line control configurations
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(2, "lines"),
    
    # Legend Positioning
    legend.position  = "right",
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11)
  )


comparison_plot

export_filename <- file.path(EXPORT_DIR, "Fig_LandscapeMetrics.tiff")

ggsave(file=export_filename, comparison_plot,
       units='px',width=4000,height=1500, dpi=400,compression='lzw')

##############################################################################################################
###########plot CLASS LEVEL METRICS
##############################################################################################################

# 2. Data Filtering & Label Factor Mapping
# =============================================================================

# Define clean publication labels for the grid row facets
metric_labs <- c(
  "pland" = "% area", 
  "ed"    = "Edge density"
)

# Filter out background noise and non-targeted indices
narok_class_clean <- narok_class %>%
  dplyr::filter(metric != "contig_mn" & class != "99") %>%
  dplyr::mutate(
    # Recode metric string into factored layouts to dictate top-down drawing order
    metric = factor(metric, levels = c("pland", "ed"))
  )

# =============================================================================
# 3. Robust Baseline Isolation Overlay Selection
# =============================================================================
# Secure row selections via matching vectors 
baseline_highlights <- narok_class_clean %>%
  dplyr::filter(
    (Year == 1974) |
      (Year == 2000 & policy %in% c("Private (GR-PR)", "Conservancy (GR-PR-CON)")) |
      (Year == 2010 & policy == "Conservancy (GR-PR-CON)")
  )

# =============================================================================
# 4. Data Visualization Layout (ggplot2 Matrix)
# =============================================================================
# Consistent, color-blind friendly aesthetic hex palette configurations
scenario_colors <- c(
  "Conservancy (GR-PR-CON)" = "#0072B2", # Dark Blue
  "Group ranch (GR)"        = "#009E73", # Green
  "Private (PR)"            = "#E69F00", # Orange
  "Private (GR-PR)"         = "#56B4E9", # Sky Blue
  "All policy areas"        = "#D55E00"  # Vermillion/Red
)

class_matrix_plot <- ggplot(narok_class_clean, aes(x = Year, y = value, color = policy)) +
  
  # Layer 1: Continuous temporal trajectory lines
  geom_line(aes(group = policy), size = 1.25, alpha = 0.85) +
  
  # Layer 2: Core focal observation vertices
  geom_point(size = 2.5) +
  
  # Layer 3: Overlay high-visibility Asterisks (Shape 8) tracking target historical baselines
  geom_point(
    data        = baseline_highlights, 
    aes(x = Year, y = value), 
    color       = "black", 
    size        = 5, 
    shape       = 8,
    show.legend = FALSE
  ) +
  
  # Structural Color Enforcements & Horizontal Legend column distribution limits
  scale_color_manual(values = scenario_colors) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  
  # Split Canvas: Rows = Configuration Metric, Columns = Ecological Class Type
  facet_grid(
    metric ~ class, 
    scales   = "free_y", 
    labeller = labeller(metric = as_labeller(metric_labs))
  ) +
  
  # Presentation Label Controls
  labs(
    y     = "Value",
    color = ""
  ) +
  
  # Theme Styling Sheet
  theme_bw(base_size = 14) +
  theme(
    # Format facet matrix boundary headers
    strip.background = element_blank(),
    strip.text       = element_text(size = 14, color = "black", face = "bold"),
    strip.text.y     = element_text(size = 13, face = "bold", angle = -90),
    
    # Typography details
    text             = element_text(family = "Cambria"),
    axis.text.x      = element_text(color = "black", size = 12),
    axis.text.y      = element_text(color = "black", size = 12),
    axis.title.x     = element_blank(),
    axis.title.y      = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    
    # Grid panel spacing properties
    panel.spacing    = unit(1.5, "lines"),
    
    # Lower horizontal block legend layout settings
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 12),
    legend.key        = element_rect(colour = "transparent", fill = "transparent"),
    legend.background = element_rect(fill = "transparent", colour = "transparent")
  )
class_matrix_plot
# =============================================================================
# 5. Export Archive File Payload
# =============================================================================
export_filename <- file.path(EXPORT_DIR, "Fig_ClassMetrics_ComparativeGrid.tiff")

ggsave(
  filename    = export_filename, 
  plot        = class_matrix_plot,
  units       = "px",
  width       = 4200,
  height      = 3200, 
  dpi         = 400,
  compression = "lzw" # LZW compression prevents file bloating inside the GitHub repo
)
##############################################################################################################
###########plot CLASS PROPORTIONS
##############################################################################################################
# =============================================================================
# 2. Data Filtering & Explicit Factor Ordering
# =============================================================================

# Dictionary definitions to force exact left-to-right panel plotting order
policy_order <- c(
  "Group ranch (GR)"        = "GR",
  "Private (PR)"            = "PR",
  "Private (GR-PR)"         = "GR-PR",
  "Conservancy (GR-PR-CON)" = "GR-PR-CON",
  "All policy areas"        = "All policy areas"
)

# Filter down to targeted metrics and recode labels cleanly in one step
narok_pland <- narok_class %>%
  dplyr::filter(metric == "pland") %>%
  dplyr::select(policy, Year, class, value) %>%
  dplyr::filter(policy %in% names(policy_order)) %>%
  dplyr::mutate(
    # Set explicit panel titles via factored levels matching your target layout
    policy = factor(policy_order[policy], levels = policy_order),
    # Force chronological string representation for x-axis scaling
    Year   = as.character(Year),
    # Force consistent land class stacking sequence
    class  = factor(class, levels = c("Cropland", "Forest", "Rangeland" ))
  )

# =============================================================================
# 3. Data Visualization Layout (Stacked Grid Framework)
# =============================================================================
# Color lookup palette matching distinct ecological land covers
class_colors <- c(
  "Cropland"  = "#D89382", # Rosy/Light Brick
  "Forest"    = "#68AA63", # Soft Green
  "Rangeland" = "#DBD83D"  # Mustard Yellow
)

year_plots <- ggplot(narok_pland, aes(x = Year, y = value, fill = class)) +
  
  # Layer 1: Stacked compositional counts normalized to uniform width bounds
  geom_bar(stat = "identity", position = "stack", width = 0.75) +
  
  # Layer 2: Facet slicing arranged linearly across a single row matrix
  facet_wrap(~policy, nrow = 1) + 
  
  # Structural Canvas limits and label specifications
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), limits = c(0, 101)) +
  #scale_fill_manual(values = c("#D89382","#68AA63", "#DBD83D"), labels = c("Cropland", "Forest", "Rangeland")) +
  scale_fill_manual(values = class_colors) +
  
  labs(
    y    = expression("% cover"),
    x    = "",
    fill = ""
  ) +
  
  # Unified Technical Document Theme Layer
  theme_minimal(base_size = 14) +
  theme(
    # Typography configurations
    text             = element_text(family = "Cambria"),
    axis.title.y     = element_text(face = "plain", size = 16, margin = margin(r = 10)),
    axis.text.y      = element_text(face = "plain", size = 14, color = "black"),
    axis.text.x      = element_text(face = "plain", size = 12, color = "black", angle = 90, vjust = 0.5, hjust = 1),
    
    # Grid panel facet header properties
    strip.text       = element_text(size = 13, face = "bold", color = "black", hjust = 0),
    strip.background = element_rect(fill = "white", colour = "transparent"),
    
    # Outer boundaries and baseline background parameters
    panel.background = element_rect(fill = "white", colour = "transparent"),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.spacing    = unit(1, "lines"),
    
    # Internal baseline guide reference lines (Replaces deprecated size parameter)
    panel.grid.major = element_line(linewidth = 0.4, linetype = "dotted", color = "grey70"),
    panel.grid.minor = element_line(linewidth = 0.4, linetype = "dotted", color = "grey85"),
    
    # Horizontal legend parameters located below drawing area
    legend.position  = "bottom",
    legend.text      = element_text(size = 13),
    legend.key.size  = unit(0.8, "cm"),
    plot.margin      = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm")
  )

# =============================================================================
# 4. Canvas Compression and Local Archive Export
# =============================================================================
export_filename <- file.path(EXPORT_DIR, "Fig_LULC_ClassProportions.tiff")

ggsave(
  filename    = export_filename, 
  plot        = year_plots,
  units       = "px",
  width       = 4500,
  height      = 2200, 
  dpi         = 500,
  compression = "lzw" # LZW compression preserves quality while protecting repo space
)
##############################################################################################################
