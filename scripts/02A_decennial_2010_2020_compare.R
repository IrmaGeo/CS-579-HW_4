# CS579 HW4 — 02A_decennial_2010_2020_compare.R
# Compare 2010 vs 2020 decennial at BLOCK-GROUP level for CA30 + neighbors
# Outputs:
#   - data/decennial_compare/decennial_CA_change_2010_2020_CA_30.csv
#   - map_2010_pct_hisp.png, map_2020_pct_hisp.png, map_2010_pct_black.png, map_2020_pct_black.png

options(tigris_use_cache = TRUE, scipen = 999)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tidycensus)
  library(readr)
  library(stringr)
  library(scales)
  library(tigris)
})

sf_use_s2(TRUE)

# 0) Parameters & paths

MY_CA_NUM      <- "30"
GEO_DIR        <- "data/geo_scaffold"
OUT_DIR        <- "data/decennial_compare"
CA_SHAPE_PATH  <- "/Users/irmamodzgvrishvili/Desktop/Education/Illinois/Fall25/CS579/Assignement_4/Boundaries - Community Areas_20251024"

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Load the 2020 BG scaffold from step 01
bg_path <- file.path(GEO_DIR, paste0("bg_target_CA_", MY_CA_NUM, ".gpkg"))
stopifnot(file.exists(bg_path))

bg_target_sf <- st_read(bg_path, quiet = TRUE) %>% st_transform(4326)
bg_geoids    <- bg_target_sf$GEOID
message("Loaded ", length(bg_geoids), " BGs for CA ", MY_CA_NUM, " + neighbors.")

# Study area polygon. Used to crop 2010 BGs
study_area <- st_union(bg_target_sf)

# Load CA boundaries
chi_ca_sf <- st_read(CA_SHAPE_PATH, quiet = TRUE) %>%
  st_transform(4326) %>%
  select(area_numbe, community) %>%
  mutate(area_numbe = as.character(area_numbe))

# CRS-safe CA tagging: return ONLY GEOID, CA_Number, CA_Name
assign_to_ca <- function(sf_layer, ca_sf) {
  local_crs <- 26916 # NAD83 / UTM zone 16N ~ Chicago
  pts <- st_transform(sf_layer, local_crs) |> st_point_on_surface()
  ca_proj <- st_transform(ca_sf, local_crs)

  lab <- st_join(pts, ca_proj, join = st_intersects) |>
    st_drop_geometry() |>
    transmute(
      GEOID   = sf_layer$GEOID,           # keep only id + CA labels
      CA_Number = area_numbe,
      CA_Name   = community
    )
  lab
}

# 1) Variable codes (explicit)
# 2020 PL
v20_total <- "P1_001N"  # Total population
v20_hisp  <- "P2_002N"  # Hispanic or Latino
v20_black <- "P1_003N"  # Black or African American alone

# 2010 SF1
v10_total <- "P001001"  # Total population
v10_hisp  <- "P004002"  # Hispanic or Latino
v10_black <- "P003003"  # Black or African American alone

message("2020/PL  total=", v20_total, "  hisp=", v20_hisp, "  black=", v20_black)
message("2010/SF1 total=", v10_total, "  hisp=", v10_hisp, "  black=", v10_black)

# 2) TIGER geometries (pull separately)
geo20 <- tigris::block_groups(state = "IL", county = "Cook", year = 2020, class = "sf") %>%
  st_transform(4326)

geo10 <- tigris::block_groups(state = "IL", county = "Cook", year = 2010, class = "sf") %>%
  st_transform(4326)

# Standardize 2010 GEOID column name for joins
if ("GEOID10" %in% names(geo10) && !"GEOID" %in% names(geo10)) {
  geo10 <- dplyr::rename(geo10, GEOID = GEOID10)
}

# 3) Decennial attributes WITHOUT geometry (force wide names), then join to TIGER

# 2020 PL attributes (wide with final names)
dec20_wide <- tidycensus::get_decennial(
  geography = "block group",
  variables = c(Total20 = v20_total, Hisp20 = v20_hisp, Black20 = v20_black),
  state = "IL", county = "Cook",
  year = 2020, dataset = "pl",
  geometry = FALSE, output = "wide"
)

# Normalize 2020 column names defensively
n20 <- names(dec20_wide)
if (!"Total20" %in% n20) {
  cand <- intersect(c("Total20","Total","P1_001N"), n20)
  if (length(cand) == 1) dec20_wide <- dplyr::rename(dec20_wide, Total20 = !!cand)
}
if (!"Hisp20" %in% n20) {
  cand <- intersect(c("Hisp20","Hisp","P2_002N"), n20)
  if (length(cand) == 1) dec20_wide <- dplyr::rename(dec20_wide, Hisp20 = !!cand)
}
if (!"Black20" %in% n20) {
  cand <- intersect(c("Black20","Black","P1_003N"), n20)
  if (length(cand) == 1) dec20_wide <- dplyr::rename(dec20_wide, Black20 = !!cand)
}
if (!"PctHisp20" %in% names(dec20_wide)) {
  dec20_wide <- dec20_wide |>
    mutate(PctHisp20  = if_else(Total20 > 0, Hisp20  / Total20, NA_real_))
}
if (!"PctBlack20" %in% names(dec20_wide)) {
  dec20_wide <- dec20_wide |>
    mutate(PctBlack20 = if_else(Total20 > 0, Black20 / Total20, NA_real_))
}

# Join to 2020 geometry and keep only scaffold BGs
dec20_sf <- geo20 %>%
  left_join(dec20_wide, by = "GEOID") %>%
  filter(GEOID %in% bg_geoids)

# 2010 SF1 attributes (wide with final names)
dec10_wide <- tidycensus::get_decennial(
  geography = "block group",
  variables = c(Total10 = v10_total, Hisp10 = v10_hisp, Black10 = v10_black),
  state = "IL", county = "Cook",
  year = 2010, dataset = "sf1",
  geometry = FALSE, output = "wide"
)

# Normalize 2010 column names defensively
n10 <- names(dec10_wide)
if (!"Total10" %in% n10) {
  cand <- intersect(c("Total10","Total","P001001"), n10)
  if (length(cand) == 1) dec10_wide <- dplyr::rename(dec10_wide, Total10 = !!cand)
}
if (!"Hisp10" %in% n10) {
  cand <- intersect(c("Hisp10","Hisp","P004002"), n10)
  if (length(cand) == 1) dec10_wide <- dplyr::rename(dec10_wide, Hisp10 = !!cand)
}
if (!"Black10" %in% n10) {
  cand <- intersect(c("Black10","Black","P003003"), n10)
  if (length(cand) == 1) dec10_wide <- dplyr::rename(dec10_wide, Black10 = !!cand)
}
if (!"PctHisp10" %in% names(dec10_wide)) {
  dec10_wide <- dec10_wide |>
    mutate(PctHisp10  = if_else(Total10 > 0, Hisp10  / Total10, NA_real_))
}
if (!"PctBlack10" %in% names(dec10_wide)) {
  dec10_wide <- dec10_wide |>
    mutate(PctBlack10 = if_else(Total10 > 0, Black10 / Total10, NA_real_))
}

# Join to 2010 geometry, then spatially crop to the same study area
dec10_sf <- geo10 %>%
  left_join(dec10_wide, by = "GEOID") %>%
  st_filter(study_area, .predicate = st_intersects)

# 4) Attach CA labels (projected point_on_surface → CA), keeping ONLY label cols

labs20 <- assign_to_ca(dec20_sf, chi_ca_sf) %>% select(GEOID, CA_Number, CA_Name)
labs10 <- assign_to_ca(dec10_sf, chi_ca_sf) %>% select(GEOID, CA_Number, CA_Name)

dec20_sf <- dec20_sf %>% left_join(labs20, by = "GEOID")
dec10_sf <- dec10_sf %>% left_join(labs10, by = "GEOID")

# 5) Sanity checks (informative)

need20 <- c("GEOID","CA_Number","Total20","Hisp20","Black20","PctHisp20","PctBlack20")
need10 <- c("GEOID","CA_Number","Total10","Hisp10","Black10","PctHisp10","PctBlack10")

has20 <- need20 %in% names(dec20_sf)
has10 <- need10 %in% names(dec10_sf)

if (!all(has20)) {
  stop("2020 missing columns: ", paste(need20[!has20], collapse = ", "),
       "\nHave: ", paste(names(dec20_sf), collapse = ", "))
}
if (!all(has10)) {
  stop("2010 missing columns: ", paste(need10[!has10], collapse = ", "),
       "\nHave: ", paste(names(dec10_sf), collapse = ", "))
}

# save tidy snapshots for transparency
write_csv(st_drop_geometry(dec20_sf), file.path(OUT_DIR, paste0("BG_2020_tidy_CA_", MY_CA_NUM, ".csv")))
write_csv(st_drop_geometry(dec10_sf), file.path(OUT_DIR, paste0("BG_2010_tidy_CA_", MY_CA_NUM, ".csv")))

# 6) CA-level rollups & changes (2010 → 2020)
ca20 <- dec20_sf %>%
  st_drop_geometry() %>%
  group_by(CA_Number, CA_Name) %>%
  summarise(
    Total20 = sum(Total20, na.rm = TRUE),
    Hisp20  = sum(Hisp20,  na.rm = TRUE),
    Black20 = sum(Black20, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PctHisp20  = if_else(Total20 > 0, Hisp20  / Total20, NA_real_),
    PctBlack20 = if_else(Total20 > 0, Black20 / Total20, NA_real_)
  )

ca10 <- dec10_sf %>%
  st_drop_geometry() %>%
  group_by(CA_Number, CA_Name) %>%
  summarise(
    Total10 = sum(Total10, na.rm = TRUE),
    Hisp10  = sum(Hisp10,  na.rm = TRUE),
    Black10 = sum(Black10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PctHisp10  = if_else(Total10 > 0, Hisp10  / Total10, NA_real_),
    PctBlack10 = if_else(Total10 > 0, Black10 / Total10, NA_real_)
  )

ca_change <- full_join(ca10, ca20, by = c("CA_Number", "CA_Name")) %>%
  mutate(
    dTotal    = Total20 - Total10,
    dPctHisp  = PctHisp20  - PctHisp10,
    dPctBlack = PctBlack20 - PctBlack10
  ) %>%
  arrange(desc(CA_Number))

write_csv(ca_change, file.path(OUT_DIR, paste0("decennial_CA_change_2010_2020_CA_", MY_CA_NUM, ".csv")))
message("✅ Saved CA-level change table to ", OUT_DIR)

# 7) BG-level maps for each year
make_pct_map <- function(data_sf, var, title, filename) {
  g <- ggplot(data_sf) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    scale_fill_continuous(labels = scales::percent, name = NULL) +
    labs(title = title, caption = "Block-group level; extent = CA30 + neighbors") +
    theme_minimal()
  ggsave(file.path(OUT_DIR, filename), g, width = 8, height = 6, dpi = 150)
}

make_pct_map(dec10_sf, "PctHisp10",  "2010 % Hispanic (BGs)", "map_2010_pct_hisp.png")
make_pct_map(dec20_sf, "PctHisp20",  "2020 % Hispanic (BGs)", "map_2020_pct_hisp.png")
make_pct_map(dec10_sf, "PctBlack10", "2010 % Black (BGs)",   "map_2010_pct_black.png")
make_pct_map(dec20_sf, "PctBlack20", "2020 % Black (BGs)",   "map_2020_pct_black.png")

message("🎉 Done. Check outputs in: ", OUT_DIR)