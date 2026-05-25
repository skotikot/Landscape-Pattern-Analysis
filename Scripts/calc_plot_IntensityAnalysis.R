# =============================================================================
# Project: Landscape pattern analysis
# Purpose: Execute OpenLand Multi-Temporal Intensity Analysis across Policy Scenarios 
# and generate visualization
# Author: Kotikot et al. (May 2026)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Load Core Required Libraries
# -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(terra)       # Core engine for structural raster logic
library(raster)      # Required safely for legacy OpenLand spatial structure compatibility
library(OpenLand)    # Multi-temporal intensity landscape analysis engine
library(ggplot2)
library(cowplot)
library(grid)

# -----------------------------------------------------------------------------
# 1. Configuration & Pathing Setup
# -----------------------------------------------------------------------------
DATA_DIR   <- "../Data/Processed"
OUTPUT_DIR <- "../Data/Figures"

# -----------------------------------------------------------------------------

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
# 2. Define Automation Parameters & Metadata Maps
# =============================================================================

# Exact policy keys mapping to their standard category names and viz hex codes
# Note: CON has only 2 active classes, while the others track 3 classes
policy_metadata <- list(
  "PR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Cropland",  cat_m = "Rangeland"),
  "GRPR" = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Cropland",  cat_m = "Rangeland"),
  "GR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Cropland",  cat_m = "Rangeland"),
  "CON"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Cropland",  cat_m = "Rangeland"),
  "all"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Cropland",  cat_m = "Rangeland")
)

# Initialize blank lists to collect the dynamic outputs
contingency_tables <- list()
intensity_results  <- list()

# =============================================================================
# 3. Automated Processing Loop
# =============================================================================

for (policy_prefix in names(policy_metadata)) {
  
  # CRUCIAL BUG FIX: Use strict regular expression anchors (^ and _) 
  # This guarantees "GR" doesn't accidentally grab "GRPR" layers.
  pattern <- paste0("^", policy_prefix, "_")
  matched_indices <- grep(pattern, names(narok_rasters))
  
  if (length(matched_indices) == 0) next
  
  # Extract and stack matching layers using terra
  policy_stack <- terra::rast(narok_rasters[matched_indices])
  
  # Convert to a standard raster object required by OpenLand internal structures
  # safely avoiding namespace collision crashes
  legacy_raster_obj <- raster::stack(policy_stack)
  
  # A. Compute OpenLand contingency matrix
  print(paste("Calculating contingency tables for framework:", policy_prefix))
  c_table <- OpenLand::contingencyTable(input_raster = legacy_raster_obj, pixelresolution = 30)
  
  # B. Inject descriptive category metadata factors & hex palettes dynamically
  meta <- policy_metadata[[policy_prefix]]
  c_table$tb_legend$categoryName <- factor(meta$cats)
  c_table$tb_legend$color        <- meta$cols
  
  # Archive structured table out to list tracking object
  contingency_tables[[policy_prefix]] <- c_table
  
  # C. Execute Intensity Analysis
  print(paste("Running intensity analysis for framework:", policy_prefix))
  intensity_results[[policy_prefix]] <- OpenLand::intensityAnalysis(
    dataset    = c_table, 
    category_n = meta$cat_n, 
    category_m = meta$cat_m
  )
}

#--------------------------------------------------------

f = function(x) {
  if (isS4(x)) {
    nms <- slotNames(x)
    names(nms) <- nms
    lapply(lapply(nms, slot, object=x), f)
  } else x
}

#--------------------------------------------------------

alltt <- intensity_results$all$transition_lvlGain_n
PRtt <- intensity_results$PR$transition_lvlGain_n
GRtt <- intensity_results$GR$transition_lvlGain_n
GRPRtt <- intensity_results$GRPR$transition_lvlGain_n
CONtt <- intensity_results$CON$transition_lvlGain_n

lst0 <- as.data.frame(f(alltt)[2])
lst1 <- as.data.frame(f(PRtt)[2])
lst2 <- as.data.frame(f(GRtt)[2])
lst3 <- as.data.frame(f(GRPRtt)[2])
lst4 <- as.data.frame(f(CONtt)[2])

list_data <- list(lst1,lst2,lst3,lst4, lst0)
polcy <- c("PR", "GR", "GRPR", "CON", "All")

dataF <- data.frame(matrix(ncol = 9))
colnames(dataF) <- c("Period", "From", "To", "Interval", "tKm2", "Rtin", "RUtn", "polcy", "FromTo")

tst <- list()
for (i in 1:length(list_data)){
  lst11<- list_data[[i]]
  names(lst11) <- c("Period", "From", "To", "Interval", "tKm2", "Rtin", "RUtn")
  lst11$polcy <- polcy[i]
  lst11$FromTo <- paste0(lst11$From, " - ",lst11$To)
  dataF <- rbind(dataF, lst11)
  
}

dataF <- dataF %>% 
  dplyr::mutate(tKm2 = if_else(To == "Rangeland", NA, tKm2),
                Rtin = if_else(To == "Rangeland", NA, Rtin),
                RUtn = if_else(To == "Rangeland", NA, RUtn))

#----------------------------------------------------
dataF$pf <- paste0(dataF$Period, "From ", dataF$From)

dataF <- dataF %>% 
  dplyr::mutate(pol = if_else(polcy == "PR", "b", polcy),
                pol = if_else(polcy == "GR", "a", pol),
                pol = if_else(polcy == "GRPR", "c", pol),
                pol = if_else(polcy=="CON", "d", pol),
                pol = if_else(polcy=="All", "e", pol))

the_colors <- c("#D89382","#68AA63", "#DBD83D")
names(the_colors) <- c("Cropland", "Forest", "Rangeland")

facet_names <- c(
  'b' = "PR",
  'c' = "GR-PR",
  'a' = "GR",
  'd' = "GR-PR-CON",
  'e' = "All policy areas")

chaInt_legend <- dataF |> 
  filter(!is.na(polcy)) |> 
  group_by(From, pol) |> 
  ggplot(aes(x = Period, y = Rtin, fill = From)) +
  geom_col(position = "dodge", show.legend = T) +
  scale_fill_manual(limits = names(the_colors), values = the_colors)+
  guides(fill=guide_legend(ncol=1))+
  geom_point(aes(Period, RUtn), colour = "black", size = 1, shape=3, show.legend = F)+
  # geom_hline(yintercept=dataF$RUtn, linetype="dashed",
  #               color = "red", size=1)+
  #scale_fill_manual(values = c("#DBD83D", "#68AA63"), labels = c("Rangeland", "Forest")) +
  #facet_grid(~ pol, labeller = label_wrap_gen(width=10)) + 
  facet_wrap(~pol, labeller = as_labeller(facet_names),nrow=1) +
  coord_flip() +
  
  #ylim(c(0, 1.7)) +
  #labs(y = "Intensity gain of cropland (%)", x="", fill="", size = 16)+
  labs(y = "", x="", fill="", size = 16)+
  #geom_text(x=1, y=1, label="(A)", fontsize=20, fontface='bold')+
  theme_bw() +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(angle = 0, hjust = 0.47, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50",
                                        linewidth = 0.15,
                                        linetype = 2),
        panel.grid.major.x = element_line(color = "grey50",
                                          size = 0.15,
                                          linetype = 2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  geom_vline(xintercept = 1.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 2.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 3.5, color=1,linewidth = 0.2)+
  
  theme(#legend.position="none",
    legend.position = c(1.01, 0.5),
    #legend.position = c(0.71, 0.6),#4cls, c(0.86, 0.72) #c(0.71, 0.6)
    #legend.justification="left",
    legend.title=element_blank(),#("Policy types",size=12), 
    legend.text=element_text(size=9),
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.background = element_rect(fill="transparent", 
                                     size=0.5, linetype="solid"))#,
# legend.box.spacing = unit(0, "pt"),
# legend.margin=margin(0,0,0.0,0))#,
#legend.box.margin=margin(-10,-10,-10,-10))

#------------------------------------------------------------------------------

dataF$pf <- paste0(dataF$Period, "From ", dataF$From)

dataF <- dataF %>% 
  dplyr::mutate(pol = if_else(polcy == "PR", "b", polcy),
                pol = if_else(polcy == "GR", "a", pol),
                pol = if_else(polcy == "GRPR", "c", pol),
                pol = if_else(polcy=="CON", "d", pol),
                pol = if_else(polcy=="All", "e", pol))

the_colors <- c("#D89382","#68AA63", "#DBD83D")
names(the_colors) <- c("Cropland", "Forest", "Rangeland")

facet_names <- c(
  'b' = "PR",
  'c' = "GR-PR",
  'a' = "GR",
  'd' = "GR-PR-CON",
  'e' = "All policy areas")

chaInt <- dataF |> 
  filter(!is.na(polcy)) |> 
  group_by(From, pol) |> 
  ggplot(aes(x = Period, y = Rtin, fill = From)) +
  geom_col(position = "dodge", show.legend = T) +
  scale_fill_manual(limits = names(the_colors), values = the_colors)+
  guides(fill=guide_legend(ncol=1))+
  geom_point(aes(Period, RUtn), colour = "black", size = 1, shape=3, show.legend = F)+
  # geom_hline(yintercept=dataF$RUtn, linetype="dashed",
  #               color = "red", size=1)+
  #scale_fill_manual(values = c("#DBD83D", "#68AA63"), labels = c("Rangeland", "Forest")) +
  #facet_grid(~ pol, labeller = label_wrap_gen(width=10)) + 
  facet_wrap(~pol, labeller = as_labeller(facet_names),nrow=1) +
  coord_flip() +
  
  #ylim(c(0, 1.7)) +
  #labs(y = "Intensity gain of cropland (%)", x="", fill="", size = 16)+
  labs(y = "", x="", fill="", size = 16)+
  #geom_text(x=1, y=1, label="(A)", fontsize=20, fontface='bold')+
  theme_bw() +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(angle = 0, hjust = 0.47, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50",
                                        linewidth = 0.15,
                                        linetype = 2),
        panel.grid.major.x = element_line(color = "grey50",
                                          size = 0.15,
                                          linetype = 2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  geom_vline(xintercept = 1.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 2.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 3.5, color=1,linewidth = 0.2)+
  
  theme(legend.position="none",
        #legend.position = c(1.1, 0.5),
        #legend.position = c(0.71, 0.6),#4cls, c(0.86, 0.72) #c(0.71, 0.6)
        #legend.justification="left",
        legend.title=element_blank(),#("Policy types",size=12), 
        legend.text=element_text(size=9),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(fill="transparent", 
                                         size=0.5, linetype="solid"))#,
# legend.box.spacing = unit(0, "pt"),
# legend.margin=margin(0,0,0.0,0))#,
#legend.box.margin=margin(-10,-10,-10,-10))

###################################################################################################################
policy_metadata_RF <- list(
  "PR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Rangeland",  cat_m = "Forest"),
  "GRPR" = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Rangeland",  cat_m = "Forest"),
  "GR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Rangeland",  cat_m = "Forest"),
  "CON"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Rangeland",  cat_m = "Forest"),
  "all"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Rangeland",  cat_m = "Forest")
)

# Initialize blank lists to collect the dynamic outputs
contingency_tables_RF <- list()
intensity_results_RF  <- list()

# =============================================================================
# 3. Automated Processing Loop
# =============================================================================

for (policy_prefix in names(policy_metadata_RF)) {
  
  # CRUCIAL BUG FIX: Use strict regular expression anchors (^ and _) 
  # This guarantees "GR" doesn't accidentally grab "GRPR" layers.
  pattern <- paste0("^", policy_prefix, "_")
  matched_indices <- grep(pattern, names(narok_rasters))
  
  if (length(matched_indices) == 0) next
  
  # Extract and stack matching layers using terra
  policy_stack <- terra::rast(narok_rasters[matched_indices])
  
  # Convert to a standard raster object required by OpenLand internal structures
  # safely avoiding namespace collision crashes
  legacy_raster_obj <- raster::stack(policy_stack)
  
  # A. Compute OpenLand contingency matrix
  print(paste("Calculating contingency tables for framework:", policy_prefix))
  c_table <- OpenLand::contingencyTable(input_raster = legacy_raster_obj, pixelresolution = 30)
  
  # B. Inject descriptive category metadata factors & hex palettes dynamically
  meta <- policy_metadata_RF[[policy_prefix]]
  c_table$tb_legend$categoryName <- factor(meta$cats)
  c_table$tb_legend$color        <- meta$cols
  
  # Archive structured table out to list tracking object
  contingency_tables_RF[[policy_prefix]] <- c_table
  
  # C. Execute Intensity Analysis
  print(paste("Running intensity analysis for framework:", policy_prefix))
  intensity_results_RF[[policy_prefix]] <- OpenLand::intensityAnalysis(
    dataset    = c_table, 
    category_n = meta$cat_n, 
    category_m = meta$cat_m
  )
}


allttl <- intensity_results_RF$all$transition_lvlLoss_m
PRttl <- intensity_results_RF$PR$transition_lvlLoss_m
GRttl <- intensity_results_RF$GR$transition_lvlLoss_m
GRPRttl <- intensity_results_RF$GRPR$transition_lvlLoss_m
CONttl <- intensity_results_RF$CON$transition_lvlLoss_m

lst10 <- as.data.frame(f(allttl)[2])
lst11 <- as.data.frame(f(PRttl)[2])
lst12 <- as.data.frame(f(GRttl)[2])
lst13 <- as.data.frame(f(GRPRttl)[2])
lst14 <- as.data.frame(f(CONttl)[2])

list_data <- list(lst11,lst12,lst13,lst14, lst10)
polcy <- c("PR", "GR", "GRPR", "CON", "All")

dataF2 <- data.frame(matrix(ncol = 9))
colnames(dataF2) <- c("Period", "To", "From", "Interval", "tKm2", "Rtin", "RUtn", "polcy", "FromTo")

for (i in 1:length(list_data)){
  lst11<- list_data[[i]]
  names(lst11) <- c("Period", "To", "From", "Interval", "tKm2", "Rtin", "RUtn")
  lst11$polcy <- polcy[i]
  lst11$FromTo <- paste0(lst11$From, " - ",lst11$To)
  dataF2 <- rbind(dataF2, lst11)
  
}

#########plot
dataF2$pf <- paste0(dataF2$Period, "To ", dataF2$To)

dataF2 <- dataF2 %>% 
  dplyr::mutate(pol = if_else(polcy == "PR", "b", polcy),
                pol = if_else(polcy == "GR", "a", pol),
                pol = if_else(polcy == "GRPR", "c", pol),
                pol = if_else(polcy=="CON", "d", pol),
                pol = if_else(polcy=="All", "e", pol))

chaInt2 <- dataF2 |> 
  filter(!is.na(polcy)) |> 
  group_by(To, pol) |> 
  ggplot(aes(x = Period, y = Rtin, fill = To)) +
  geom_col(position = "dodge", show.legend = F) +
  scale_fill_manual(values = c("#D89382", "#DBD83D"), labels = c("Cropland", "Rangeland")) +
  geom_point(aes(Period, RUtn), colour = "black", size = 1, shape=3, show.legend = F)+
  #facet_grid(Period + From ~ polcy)+
  facet_grid(~ pol, labeller = label_wrap_gen(width=10)) + 
  coord_flip() +
  #ylim(c(0, 1.7)) +
  #labs(y = "Intensity loss of forest (%)", x="", fill="", size = 16)+
  
  labs(y = "", x="", fill="", size = 16)+
  theme_bw() +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(angle = 0, hjust = 0.47, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50",
                                        linewidth = 0.15,
                                        linetype = 2),
        panel.grid.major.x = element_line(color = "grey50",
                                          size = 0.15,
                                          linetype = 2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  geom_vline(xintercept = 1.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 2.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 3.5, color=1,linewidth = 0.2)

chaInt2

###################################################################################################################
policy_metadata_FR <- list(
  "PR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Forest",  cat_m = "Rangeland"),
  "GRPR" = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Forest",  cat_m = "Rangeland"),
  "GR"   = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Forest",  cat_m = "Rangeland"),
  "CON"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Forest",  cat_m = "Rangeland"),
  "all"  = list(cats = c("Cropland", "Forest", "Rangeland"), cols = c("#D89382", "#68AA63", "#DBD83D"), cat_n = "Forest",  cat_m = "Rangeland")
)

# Initialize blank lists to collect the dynamic outputs
contingency_tables_FR <- list()
intensity_results_FR  <- list()

# =============================================================================
# 3. Automated Processing Loop
# =============================================================================
print("Executing OpenLand pipelines across policy frameworks...")

for (policy_prefix in names(policy_metadata_FR)) {
  
  # CRUCIAL BUG FIX: Use strict regular expression anchors (^ and _) 
  # This guarantees "GR" doesn't accidentally grab "GRPR" layers.
  pattern <- paste0("^", policy_prefix, "_")
  matched_indices <- grep(pattern, names(narok_rasters))
  
  if (length(matched_indices) == 0) next
  
  # Extract and stack matching layers using terra
  policy_stack <- terra::rast(narok_rasters[matched_indices])
  
  # Convert to a standard raster object required by OpenLand internal structures
  # safely avoiding namespace collision crashes
  legacy_raster_obj <- raster::stack(policy_stack)
  
  # A. Compute OpenLand contingency matrix
  print(paste("Calculating contingency tables for framework:", policy_prefix))
  c_table <- OpenLand::contingencyTable(input_raster = legacy_raster_obj, pixelresolution = 30)
  
  # B. Inject descriptive category metadata factors & hex palettes dynamically
  meta <- policy_metadata_FR[[policy_prefix]]
  c_table$tb_legend$categoryName <- factor(meta$cats)
  c_table$tb_legend$color        <- meta$cols
  
  # Archive structured table out to list tracking object
  contingency_tables_FR[[policy_prefix]] <- c_table
  
  # C. Execute Intensity Analysis
  print(paste("Running intensity analysis for framework:", policy_prefix))
  intensity_results_FR[[policy_prefix]] <- OpenLand::intensityAnalysis(
    dataset    = c_table, 
    category_n = meta$cat_n, 
    category_m = meta$cat_m
  )
}

allttlr <- intensity_results_FR$all$transition_lvlLoss_m
PRttlr <- intensity_results_FR$PR$transition_lvlLoss_m
GRttlr <- intensity_results_FR$GR$transition_lvlLoss_m
GRPRttlr <- intensity_results_FR$GRPR$transition_lvlLoss_m
CONttlr <- intensity_results_FR$CON$transition_lvlLoss_m
# 

lst0 <- as.data.frame(f(allttlr)[2])
lst1 <- as.data.frame(f(PRttlr)[2])
lst2 <- as.data.frame(f(GRttlr)[2])
lst3 <- as.data.frame(f(GRPRttlr)[2])
lst4 <- as.data.frame(f(CONttlr)[2])

list_data <- list(lst1,lst2,lst3,lst4, lst0)
polcy <- c("PR", "GR", "GRPR", "CON", "All")

dataFr <- data.frame(matrix(ncol = 9))
colnames(dataFr) <- c("Period", "To", "From", "Interval", "tKm2", "Rtin", "RUtn", "polcy", "FromTo")

tst <- list()
for (i in 1:length(list_data)){
  lst11<- list_data[[i]]
  names(lst11) <- c("Period", "To", "From", "Interval", "tKm2", "Rtin", "RUtn")
  lst11$polcy <- polcy[i]
  lst11$FromTo <- paste0(lst11$From, " - ",lst11$To)
  dataFr <- rbind(dataFr, lst11)
  
}

###PLOT
dataFr$pf <- paste0(dataFr$Period, "To ", dataFr$To)

dataFr <- dataFr %>% 
  dplyr::mutate(pol = if_else(polcy == "PR", "b", polcy),
                pol = if_else(polcy == "GR", "a", pol),
                pol = if_else(polcy == "GRPR", "c", pol),
                pol = if_else(polcy=="CON", "d", pol),
                pol = if_else(polcy=="All", "e", pol))

chaIntr <- dataFr |> 
  filter(!is.na(polcy)) |> 
  group_by(To, pol) |> 
  ggplot(aes(x = Period, y = Rtin, fill = To)) +
  geom_col(position = "dodge", show.legend = F) +
  scale_fill_manual(values = c("#D89382", "#68AA63" ), labels = c("Cropland", "Forest")) +
  geom_point(aes(Period, RUtn), colour = "black", size = 1, shape=3, show.legend = F)+
  #facet_grid(Period + From ~ polcy)+
  facet_grid(~ pol, labeller = label_wrap_gen(width=10)) + 
  coord_flip() +
  #ylim(c(0, 1.7)) +
  labs(y = "Intensity (% per year)", x="", fill="", size = 16)+
  #labs(y = "Intensity loss of Rangeland (%)", x="", fill="", size = 16)+
  
  theme_bw() +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(angle = 0, hjust = 0.47, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50",
                                        linewidth = 0.15,
                                        linetype = 2),
        panel.grid.major.x = element_line(color = "grey50",
                                          size = 0.15,
                                          linetype = 2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  geom_vline(xintercept = 1.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 2.5, color=1,linewidth = 0.2)+
  geom_vline(xintercept = 3.5, color=1,linewidth = 0.2)

chaIntr

###########################################################################################################

grobs <- ggplotGrob(chaInt_legend)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# build grid without legends
ff3=plot_grid(chaInt, chaInt2, chaIntr, align = "h",nrow = 3)

# add legend
ff3 <- plot_grid(ff3, legend, ncol = 2, rel_widths = c(1, .1), rel_heights = c(.1, 1))+
  
  annotate("text",x=0.24,y=1, size=4,label="(a) Cropland gain from forest and rangeland") +
  annotate("text",x=0.24,y=0.65, size=4,label="(b) Forest loss to cropland and rangeland") +
  annotate("text",x=0.24,y=0.32, size=4,label="(c) Rangeland loss to cropland and forest") +
  
  
  # # Add some space around the edges  
  theme(plot.margin = unit(c(0.2,1.2,0.4,0), "cm"))
#theme(plot.margin = unit(c(0.2,0,0.2,0.4), "cm")) 
#theme(plot.margin = unit(c(0.2,0,0.2,0.0), "cm")) 
# 
n <- c("(a) Cropland gain from forest and rangeland",
       "(b) Forest loss to cropland and rangeland",
       "(c) Rangeland loss to cropland and forest")

# # Have to turn off clipping
gt <- ggplot_gtable(ggplot_build(ff3))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
# # 
# # need to draw it with the new clip settings
ff3

export_filename <- file.path(OUTPUT_DIR, "Fig_changeIntensity.tiff")

ggsave(export_filename, ff3, units='px',width=4000,height=3000, dpi=600,compression='lzw')

