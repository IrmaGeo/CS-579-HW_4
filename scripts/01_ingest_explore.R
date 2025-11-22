# CS579 Final Project — 01_ingest_explore.R
# Build a ≥60 BG scaffold for CA 30 (South Lawndale) + neighbors
#
# Outputs (under project_root/data/raw/geo_scaffold_CA30/):
#   - bg_target_CA_30.gpkg      (BG geometry + CA labels)
#   - bg_geoids_CA_30.csv       (BG IDs)
#   - bg_counts_CA_30.csv       (BG count per CA)
#   - target_cas_for_CA_30.txt  (CA IDs used to reach ≥60 BGs)
#   - scaffold_map_CA_30.png    (map)

options(tigris_use_cache = TRUE, scipen = 999)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tigris)
  library(readr)
  library(tibble)
})

sf_use_s2(TRUE)

# -------------------------------------------------------------------
# 1) Parameters & Paths
# -------------------------------------------------------------------

MY_CA_NUM  <- "30"
MY_CA_NAME <- "SOUTH LAWNDALE"

# TODO: if you later copy the shapefile into data/raw/, update this path.
CA_SHAPE_PATH <- "/Users/irmamodzgvrishvili/Desktop/Education/Illinois/Fall25/CS579/Assignement_4/Boundaries - Community Areas_20251024/geo_export_8d7b0465-2b0f-4e61-ad5c-aac8fbd3e33d.shp"

# New folder structure: put scaffold under data/raw/geo_scaffold_CA30
DATA_DIR          <- "data"
DATA_RAW_DIR      <- file.path(DATA_DIR, "raw")
GEO_SCAFFOLD_DIR  <- file.path(DATA_DIR, "geo_scaffold_CA30")

# Create directories if they don't exist
for (d in c(DATA_DIR, DATA_RAW_DIR, GEO_SCAFFOLD_DIR)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

OUT_DIR <- GEO_SCAFFOLD_DIR

# -------------------------------------------------------------------
# 2) Load Community Areas
# -------------------------------------------------------------------

chi_ca_sf <- st_read(CA_SHAPE_PATH, quiet = TRUE) |>
  st_transform(4326) |>
  select(area_numbe, community) |>
  mutate(area_numbe = as.character(area_numbe))

stopifnot(all(c("area_numbe", "community") %in% names(chi_ca_sf)))
cat("Loaded Community Areas:", nrow(chi_ca_sf), "polygons\n")

# -------------------------------------------------------------------
# 3) Block groups + blocks geometry
# -------------------------------------------------------------------

bg_2020 <- tigris::block_groups(
  state = "IL", county = "Cook",
  year = 2020, class = "sf"
) |>
  st_transform(4326) |>
  select(GEOID, geometry)
cat("BG 2020 (Cook):", nrow(bg_2020), "features\n")

blocks_2010 <- tigris::blocks(
  state = "IL", county = "Cook",
  year = 2010, class = "sf"
) |>
  st_transform(4326) |>
  select(GEOID10, geometry)
cat("Blocks 2010 (Cook):", nrow(blocks_2010), "features\n")

# -------------------------------------------------------------------
# 4) Neighbor CAs & CA assignment helpers
# -------------------------------------------------------------------

get_neighbors <- function(seed_ids, ca_sf) {
  touched <- st_touches(
    ca_sf,
    ca_sf |> dplyr::filter(area_numbe %in% seed_ids),
    sparse = FALSE
  )
  ca_sf$area_numbe[rowSums(touched) > 0]
}

# Assign each geometry to a CA (centroid in projected CRS)
assign_to_ca <- function(sf_layer, ca_sf) {
  id_col    <- names(sf_layer)[1]
  local_crs <- 26916  # UTM zone 16N (Chicago)

  pts     <- st_transform(sf_layer, local_crs) |> st_point_on_surface()
  ca_proj <- st_transform(ca_sf, local_crs)

  st_join(pts, ca_proj, join = st_intersects) |>
    st_drop_geometry() |>
    dplyr::rename(CA_Number = area_numbe, CA_Name = community) |>
    dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
}

# -------------------------------------------------------------------
# 5) Build ≥60 BG scaffold for CA 30 + neighbors
# -------------------------------------------------------------------

TARGET_CA_NUMBERS <- MY_CA_NUM
iter <- 0

repeat {
  bg_lookup <- assign_to_ca(bg_2020, chi_ca_sf) |>
    dplyr::rename(GEOID = GEOID)

  n_bg <- bg_lookup |>
    dplyr::filter(CA_Number %in% TARGET_CA_NUMBERS) |>
    nrow()

  cat("Iteration", iter, "- BG count in target set:", n_bg, "\n")

  if (n_bg >= 60 || iter > 5) break

  TARGET_CA_NUMBERS <- unique(c(
    TARGET_CA_NUMBERS,
    get_neighbors(TARGET_CA_NUMBERS, chi_ca_sf)
  ))
  iter <- iter + 1
}

cat("Included CA IDs:", paste(TARGET_CA_NUMBERS, collapse = ", "), "\n")

bg_target_ids <- bg_lookup |>
  dplyr::filter(CA_Number %in% TARGET_CA_NUMBERS) |>
  dplyr::pull(GEOID)

bg_target_sf <- bg_2020 |>
  dplyr::filter(GEOID %in% bg_target_ids) |>
  dplyr::left_join(
    bg_lookup |> dplyr::select(GEOID, CA_Number, CA_Name),
    by = "GEOID"
  )

bg_counts <- bg_target_sf |>
  st_drop_geometry() |>
  dplyr::count(CA_Number, name = "BG_Count") |>
  dplyr::arrange(desc(BG_Count)) |>
  tibble::as_tibble()

print(bg_counts, n = 100)

# -------------------------------------------------------------------
# 6) Save scaffold (files overwrite if they exist)
# -------------------------------------------------------------------

write_lines(
  TARGET_CA_NUMBERS,
  file.path(OUT_DIR, paste0("target_cas_for_CA_", MY_CA_NUM, ".txt"))
)

st_write(
  bg_target_sf,
  dsn = file.path(OUT_DIR, paste0("bg_target_CA_", MY_CA_NUM, ".gpkg")),
  layer_options = "OVERWRITE=YES",
  quiet = TRUE
)

bg_target_sf |>
  st_drop_geometry() |>
  dplyr::arrange(CA_Number, GEOID) |>
  readr::write_csv(file.path(OUT_DIR, paste0("bg_geoids_CA_", MY_CA_NUM, ".csv")))

readr::write_csv(
  bg_counts,
  file.path(OUT_DIR, paste0("bg_counts_CA_", MY_CA_NUM, ".csv"))
)

cat("Saved scaffold to", OUT_DIR, "\n")

# -------------------------------------------------------------------
# 7) Visualization
# -------------------------------------------------------------------

base_map      <- chi_ca_sf |> dplyr::filter(area_numbe %in% TARGET_CA_NUMBERS)
focus_outline <- base_map |> dplyr::filter(area_numbe == MY_CA_NUM)

p_scaffold <- ggplot() +
  geom_sf(data = base_map, fill = "grey95", color = "grey70") +
  geom_sf(data = bg_target_sf, fill = "steelblue", color = NA, alpha = 0.45) +
  geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1) +
  labs(
    title    = paste0("BG Scaffold for CA ", MY_CA_NUM, " (≥ 60 BGs)"),
    subtitle = paste("Included CAs:", paste(TARGET_CA_NUMBERS, collapse = ", "))
  ) +
  theme_minimal()

ggsave(
  filename = file.path(OUT_DIR, paste0("scaffold_map_CA_", MY_CA_NUM, ".png")),
  plot = p_scaffold, width = 8, height = 6, dpi = 150
)

cat("Saved map to", file.path(OUT_DIR, paste0("scaffold_map_CA_", MY_CA_NUM, ".png")), "\n")