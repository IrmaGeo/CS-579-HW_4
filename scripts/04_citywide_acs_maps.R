# ============================================================
# CS579 Final Project — 04_citywide_acs_maps.R
# Citywide ACS Maps for socio-economic variables
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(readr)

sf_use_s2(TRUE)

# ------------------------------------------------------------
# 1. Load cleaned dataset (BG-level, all Chicago)
# ------------------------------------------------------------
clean_path <- file.path("data", "clean_data", "CA_ALL_clean.rds")
bg <- read_rds(clean_path)

stopifnot("GEOID" %in% names(bg))

# Output folder for maps
OUT_DIR <- file.path("results", "figures", "citywide_acs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. Variables to plot (must exist in cleaned dataset)
# ------------------------------------------------------------
var_list <- c(
  "PctWhite", "PctBlack", "PctHisp",
  "PctCollege",
  "MedHHIE",
  "PctPoverty",
  "UnemploymentRate",
  "MedRentE",
  "PctRenter"
)

# Pretty titles for maps
pretty_names <- c(
  PctWhite = "% White",
  PctBlack = "% Black",
  PctHisp = "% Hispanic",
  PctCollege = "% College Degree",
  MedHHIE = "Median Household Income",
  PctPoverty = "% Poverty",
  UnemploymentRate = "% Unemployment",
  MedRentE = "Median Rent",
  PctRenter = "% Renter-Occupied"
)

# ------------------------------------------------------------
# 3. Plotting function
# ------------------------------------------------------------
plot_var <- function(data, var, pretty_title, out_path) {
  gg <- ggplot(data) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    scale_fill_viridis(
      option = "magma",
      direction = -1,
      name = pretty_title,
      na.value = "grey85"
    ) +
    labs(
      title = pretty_title,
      subtitle = "Chicago Block Groups — ACS 2018–2022",
      caption = "CS579 Final Project — Irma Modzgvrishvili"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(out_path, gg, width = 9, height = 6.5, dpi = 200)
}

# ------------------------------------------------------------
# 4. Loop through variables and generate maps
# ------------------------------------------------------------
for (v in var_list) {
  if (!(v %in% names(bg))) {
    message("⚠️ Skipping ", v, " — not found in dataset.")
    next
  }
  
  outfile <- file.path(OUT_DIR, paste0("map_citywide_", v, ".png"))
  plot_var(bg, v, pretty_names[[v]], outfile)
  message("Saved: ", outfile)
}

message("✅ All citywide ACS maps generated at: ", OUT_DIR)