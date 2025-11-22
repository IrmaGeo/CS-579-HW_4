# CS579 Final Project — 02_clean_select_variables.R
# City-wide ACS pull + feature selection & scaling
# UPDATED: Now includes Poverty & Unemployment variables so separate pull isn't needed later.

options(tigris_use_cache = TRUE, scipen = 999)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tidycensus)
  library(readr)
  library(stringr)
  library(reshape2)
  library(scales)
  library(tibble)
})

sf_use_s2(TRUE)

# -------------------------------------------------------------------
# 0) Parameters & Directory Structure
# -------------------------------------------------------------------

ACS_YEAR <- 2022  # 2018–2022 ACS (5-year)

DATA_DIR      <- "data"
TEMP_DIR      <- file.path(DATA_DIR, "temp")
CLEAN_DIR     <- file.path(DATA_DIR, "clean_data")
RAW_DIR       <- file.path(DATA_DIR, "raw")   # for shapefiles if you copy them later

dir.create(DATA_DIR,  showWarnings = FALSE)
dir.create(TEMP_DIR,  showWarnings = FALSE)
dir.create(CLEAN_DIR, showWarnings = FALSE)

# 🔥 UPDATE THIS once you store shapefile inside your project:
CA_SHAPE_PATH <- "/Users/irmamodzgvrishvili/Desktop/Education/Illinois/Fall25/CS579/Assignement_4/Boundaries - Community Areas_20251024"

# -------------------------------------------------------------------
# 1) Load Chicago Community Areas
# -------------------------------------------------------------------

chi_ca_sf <- st_read(CA_SHAPE_PATH, quiet = TRUE) |>
  st_transform(4326) |>
  select(area_numbe, community) |>
  mutate(
    area_numbe = as.character(area_numbe),
    community  = as.character(community)
  )

stopifnot(all(c("area_numbe","community") %in% names(chi_ca_sf)))
message("Loaded ", nrow(chi_ca_sf), " Chicago Community Areas.")

# Helper: assign each BG to a CA using centroid join
assign_to_ca <- function(sf_layer, ca_sf) {
  local_crs <- 26916  # UTM Chicago
  pts       <- st_transform(sf_layer, local_crs) |> st_point_on_surface()
  ca_proj   <- st_transform(ca_sf, local_crs)

  join_df <- st_join(pts, ca_proj, join = st_intersects)

  tibble(
    GEOID     = sf_layer$GEOID,
    CA_Number = join_df$area_numbe,
    CA_Name   = join_df$community
  )
}

# -------------------------------------------------------------------
# 2) Pull ACS Citywide Block-Group Data
# -------------------------------------------------------------------

acs_vars <- c(
  TotalPop   = "B01003_001",
  MedHHI     = "B19013_001",
  MedRent    = "B25064_001",
  TotalHU    = "B25003_001",
  OwnerOcc   = "B25003_002",
  RenterOcc  = "B25003_003",
  Age25Total = "B15003_001",
  BachPlus   = "B15003_022",
  Black      = "B03002_004",
  Hisp       = "B03002_012",
  White      = "B03002_003",
  # --- ADDED VARIABLES (Poverty & Unemployment) ---
  PovUniverse = "B17021_001",   # Universe: Population for whom poverty status is determined
  PovBelow    = "B17021_002",   # Income in the past 12 months below poverty level
  LaborForce  = "B23025_003",   # In civilian labor force
  Unemployed  = "B23025_005"    # Unemployed
)

message("Requesting ACS ", ACS_YEAR, " block-group data...")
acs_bg_all <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state = "IL",
  county = "Cook",
  year = ACS_YEAR,
  output = "wide",
  geometry = TRUE
) |> st_transform(4326)

message("Pulled ", nrow(acs_bg_all), " BGs (Cook County).")

# -------------------------------------------------------------------
# 3) Keep only BGs whose centroid lies inside Chicago
# -------------------------------------------------------------------

labs <- assign_to_ca(acs_bg_all, chi_ca_sf)

acs_bg <- acs_bg_all |>
  left_join(labs, by = "GEOID") |>
  filter(!is.na(CA_Number))

message("Kept ", nrow(acs_bg), " BGs inside Chicago.")

# -------------------------------------------------------------------
# 4) Derived Indicators (your core variables)
# -------------------------------------------------------------------

acs_ind <- acs_bg |>
  mutate(
    PctRenter  = if_else(TotalHUE    > 0, RenterOccE / TotalHUE, NA_real_),
    PctOwner   = if_else(TotalHUE    > 0, OwnerOccE  / TotalHUE, NA_real_),
    PctCollege = if_else(Age25TotalE > 0, BachPlusE  / Age25TotalE, NA_real_),
    PctBlack   = if_else(TotalPopE   > 0, BlackE     / TotalPopE, NA_real_),
    PctHisp    = if_else(TotalPopE   > 0, HispE      / TotalPopE, NA_real_),
    PctWhite   = if_else(TotalPopE   > 0, WhiteE     / TotalPopE, NA_real_),
    # --- ADDED INDICATORS ---
    PctPoverty       = if_else(PovUniverseE > 0, PovBelowE   / PovUniverseE, NA_real_),
    UnemploymentRate = if_else(LaborForceE  > 0, UnemployedE / LaborForceE,  NA_real_)
  )

# -------------------------------------------------------------------
# 5) EDA of Candidate Variables
# -------------------------------------------------------------------

cand_vars <- c(
  "MedHHIE","MedRentE",
  "PctRenter","PctCollege",
  "PctBlack","PctHisp","PctWhite"
)

eda_df <- acs_ind |> st_drop_geometry() |> select(GEOID, CA_Number, all_of(cand_vars))

# Missingness
miss_tbl <- eda_df |>
  summarise(across(all_of(cand_vars), ~ mean(is.na(.)))) |>
  pivot_longer(everything()) |>
  arrange(value)

write_csv(miss_tbl, file.path(TEMP_DIR, "missingness_citywide.csv"))

# Variance table
var_tbl <- eda_df |>
  summarise(across(all_of(cand_vars), sd, na.rm=TRUE)) |>
  pivot_longer(everything(), names_to="var", values_to="sd")

write_csv(var_tbl, file.path(TEMP_DIR, "variation_citywide.csv"))

# Correlation matrix
cor_mat <- eda_df |> select(all_of(cand_vars)) |> cor(use="pairwise.complete.obs")
write_csv(as.data.frame(cor_mat), file.path(TEMP_DIR, "correlation_citywide.csv"))

# -------------------------------------------------------------------
# 6) Auto-select 6 Variables (your HW4 logic)
# -------------------------------------------------------------------

pick_order <- c("MedHHIE","MedRentE","PctCollege","PctRenter","PctBlack","PctHisp","PctWhite")

ok_vars <- miss_tbl |> filter(value <= 0.10) |> pull(name)

chosen <- character(0)
for (v in pick_order) {
  if (v %in% ok_vars && all(abs(cor_mat[v, chosen]) < 0.80)) {
    chosen <- c(chosen, v)
  }
  if (length(chosen) == 6) break
}

message("Auto-selected variables: ", paste(chosen, collapse=", "))
write_lines(chosen, file.path(CLEAN_DIR, "selected_vars_citywide.txt"))

# -------------------------------------------------------------------
# 7) Drop incomplete BGs + scale
# -------------------------------------------------------------------

# We keep 'chosen' vars for modeling, PLUS the new map variables for visualization
# even if they aren't used in the k-NN model.
extra_map_vars <- c("PctPoverty", "UnemploymentRate")
# Only keep columns that actually exist (in case calculation failed)
extra_map_vars <- intersect(extra_map_vars, names(acs_ind))

model_ready <- acs_ind |>
  select(GEOID, CA_Number, CA_Name, geometry, all_of(chosen), all_of(extra_map_vars))

# Drop BGs with NA in the *chosen modeling variables* only
dropped <- model_ready |> st_drop_geometry() |> filter(if_any(all_of(chosen), is.na))
if (nrow(dropped) > 0) {
  write_csv(dropped, file.path(CLEAN_DIR, "dropped_BG_missing.csv"))
  message("Dropped ", nrow(dropped), " BGs with missing values in modeling variables.")
}

model_ready <- model_ready |> filter(if_all(all_of(chosen), ~ !is.na(.)))

# Scale ONLY the modeling variables
scaled <- model_ready |>
  st_drop_geometry() |>
  select(all_of(chosen)) |>
  scale() |>
  as.data.frame()

colnames(scaled) <- paste0(colnames(scaled), "_Z")

final_sf <- bind_cols(model_ready, scaled) |> st_as_sf()

# -------------------------------------------------------------------
# 8) Save Outputs
# -------------------------------------------------------------------

save_rds_path <- file.path(CLEAN_DIR, "CA_ALL_clean.rds")
write_rds(final_sf, save_rds_path)

write_csv(
  final_sf |> st_drop_geometry(),
  file.path(CLEAN_DIR, "CA_ALL_clean_no_geom.csv")
)

message("✅ Saved clean dataset: ", save_rds_path)

# -------------------------------------------------------------------
# 9) Quick EDA Plots
# -------------------------------------------------------------------

long_df <- final_sf |> st_drop_geometry() |> select(all_of(chosen)) |>
  pivot_longer(everything())

p_hist <- ggplot(long_df, aes(value)) +
  geom_histogram(bins=30) +
  facet_wrap(~name, scales="free") +
  theme_minimal()

ggsave(file.path(TEMP_DIR, "chosen_vars_hist.png"), p_hist, width=8, height=6)

cor_df <- melt(cor(final_sf |> st_drop_geometry() |> select(all_of(chosen))))
colnames(cor_df) <- c("x","y","r")

p_cor <- ggplot(cor_df, aes(x,y,fill=r)) +
  geom_tile() +
  geom_text(aes(label=round(r,2)), size=3) +
  scale_fill_gradient2(low="#2166AC", high="#B2182B", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

ggsave(file.path(TEMP_DIR, "chosen_vars_correlation.png"), p_cor, width=6, height=5)

message("📊 Saved EDA plots.")