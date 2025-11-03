# 02_pull_acs_eda_select.R


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

# 0) Parameters & paths
MY_CA_NUM  <- "30"
ACS_YEAR   <- 2022     # 2018–2022 ACS
GEO_DIR    <- "data/geo_scaffold"
OUT_EXP    <- "data/temp"
OUT_CLEAN  <- "data/clean_data"

dir.create(OUT_EXP,   showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_CLEAN, showWarnings = FALSE, recursive = TRUE)

# 1) Load the scaffold (from step 01)

bg_target_gpkg <- file.path(GEO_DIR, paste0("bg_target_CA_", MY_CA_NUM, ".gpkg"))
if (!file.exists(bg_target_gpkg)) {
  stop("Scaffold not found: ", bg_target_gpkg,
       "\nRun scripts/01_ingest_only.R first.")
}

bg_target_sf <- st_read(bg_target_gpkg, quiet = TRUE) %>% st_transform(4326)
bg_geoids    <- bg_target_sf$GEOID
message("Loaded scaffold with ", length(bg_geoids), " BGs (CA ", MY_CA_NUM, " + neighbors).")

# 2) Pull a broad ACS set
acs_vars_broad <- c(
  TotalPop    = "B01003_001",
  MedHHI      = "B19013_001",
  MedRent     = "B25064_001",
  TotalHU     = "B25003_001",
  OwnerOcc    = "B25003_002",
  RenterOcc   = "B25003_003",
  Age25Total  = "B15003_001",
  BachPlus    = "B15003_022",
  Black       = "B03002_004",
  Hisp        = "B03002_012",
  White       = "B03002_003"
)

acs_bg <- get_acs(
  geography = "block group",
  variables = acs_vars_broad,
  state = "IL", county = "Cook",
  year = ACS_YEAR, output = "wide", geometry = TRUE
) %>%
  st_transform(4326) %>%
  filter(GEOID %in% bg_geoids)

# 3) Attach CA labels from scaffold
acs_bg <- acs_bg %>%
  left_join(
    bg_target_sf %>% st_drop_geometry() %>% select(GEOID, CA_Number, CA_Name),
    by = "GEOID"
  )

# 4) Derived indicators (exploration stage)

acs_ind <- acs_bg %>%
  mutate(
    TotalPopE    = TotalPopE,
    MedHHIE      = MedHHIE,
    MedRentE     = MedRentE,
    TotalHUE     = TotalHUE,
    OwnerOccE    = OwnerOccE,
    RenterOccE   = RenterOccE,
    Age25TotalE  = Age25TotalE,
    BachPlusE    = BachPlusE,
    BlackE       = BlackE,
    HispE        = HispE,
    WhiteE       = WhiteE,

    # Ratios (guard denominators)
    PctRenter    = if_else(TotalHUE     > 0, RenterOccE / TotalHUE, NA_real_),
    PctOwner     = if_else(TotalHUE     > 0, OwnerOccE  / TotalHUE, NA_real_),
    PctCollege   = if_else(Age25TotalE  > 0, BachPlusE  / Age25TotalE, NA_real_),
    PctBlack     = if_else(TotalPopE    > 0, BlackE     / TotalPopE,   NA_real_),
    PctHisp      = if_else(TotalPopE    > 0, HispE      / TotalPopE,   NA_real_),
    PctWhite     = if_else(TotalPopE    > 0, WhiteE     / TotalPopE,   NA_real_)
  )

# 5) EDA: missingness, variation, correlation

cand_vars <- c("MedHHIE","MedRentE","PctRenter","PctOwner","PctCollege","PctBlack","PctHisp","PctWhite")

eda_df <- acs_ind %>% st_drop_geometry() %>%
  select(GEOID, CA_Number, all_of(cand_vars))

# Missingness
miss_tbl <- eda_df %>%
  summarise(across(all_of(cand_vars), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_rate") %>%
  arrange(na_rate)

# Variation (SD snapshot)
var_tbl <- eda_df %>%
  summarise(
    MedHHIE_sd    = sd(MedHHIE, na.rm = TRUE),
    MedRentE_sd   = sd(MedRentE, na.rm = TRUE),
    PctRenter_sd  = sd(PctRenter, na.rm = TRUE),
    PctOwner_sd   = sd(PctOwner, na.rm = TRUE),
    PctCollege_sd = sd(PctCollege, na.rm = TRUE),
    PctBlack_sd   = sd(PctBlack, na.rm = TRUE),
    PctHisp_sd    = sd(PctHisp, na.rm = TRUE),
    PctWhite_sd   = sd(PctWhite, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "sd")

# Correlation
cor_mat <- eda_df %>%
  select(all_of(cand_vars)) %>%
  cor(use = "pairwise.complete.obs")

# Save EDA outputs
write_csv(miss_tbl, file.path(OUT_EXP, paste0("eda_missingness_CA_", MY_CA_NUM, ".csv")))
write_csv(var_tbl,  file.path(OUT_EXP, paste0("eda_variation_CA_",  MY_CA_NUM, ".csv")))
write_csv(as.data.frame(cor_mat) %>% rownames_to_column("var"),
          file.path(OUT_EXP, paste0("eda_correlations_CA_", MY_CA_NUM, ".csv")))

# 6) Auto-pick 6 variables (greedy, low-missing, diverse, low |r|)
pick_order <- c("MedHHIE","MedRentE","PctCollege","PctRenter","PctBlack","PctHisp","PctOwner","PctWhite")

ok_vars <- miss_tbl %>% filter(na_rate <= 0.10) %>% pull(variable)

is_acceptable <- function(var, chosen, cor_mat, thr = 0.80) {
  if (length(chosen) == 0) return(TRUE)
  all(abs(cor_mat[var, chosen]) < thr | is.na(cor_mat[var, chosen]))
}

chosen <- character(0)
for (v in pick_order) {
  if (v %in% ok_vars && is_acceptable(v, chosen, cor_mat, thr = 0.80)) {
    chosen <- c(chosen, v)
  }
  if (length(chosen) == 6) break
}

# Fallback fill if needed
if (length(chosen) < 6) {
  sd_lookup <- sapply(cand_vars, function(nm) sd(eda_df[[nm]], na.rm = TRUE))
  remaining <- setdiff(ok_vars, chosen)
  remaining <- remaining[order(
    miss_tbl$na_rate[match(remaining, miss_tbl$variable)],
    -sd_lookup[remaining]
  )]
  for (v in remaining) {
    if (is_acceptable(v, chosen, cor_mat, thr = 0.80)) {
      chosen <- c(chosen, v)
    }
    if (length(chosen) == 6) break
  }
}

message("Auto-selected variables: ", paste(chosen, collapse = ", "))
write_lines(chosen, file.path(OUT_CLEAN, paste0("selected_vars_CA_", MY_CA_NUM, ".txt")))

# 7) drop incomplete BGs before scaling
model_ready <- acs_ind %>%
  select(GEOID, CA_Number, CA_Name, geometry, all_of(chosen))

# Log + drop rows with any NA in chosen vars
dropped <- model_ready %>%
  st_drop_geometry() %>%
  filter(if_any(all_of(chosen), is.na))

if (nrow(dropped) > 0) {
  write_csv(dropped, file.path(OUT_CLEAN, paste0("dropped_BG_missing_CA_", MY_CA_NUM, ".csv")))
  message("Dropped ", nrow(dropped), " BGs with missing values (logged to dropped_BG_missing_*.csv).")
}

model_ready <- model_ready %>%
  filter(if_all(all_of(chosen), ~ !is.na(.)))

# Scale z-scores AFTER dropping NAs
scaled_mat <- model_ready %>%
  st_drop_geometry() %>%
  select(all_of(chosen)) %>%
  scale() %>%
  as.data.frame()
colnames(scaled_mat) <- paste0(colnames(scaled_mat), "_Z")

final_sf <- bind_cols(model_ready, scaled_mat) %>% st_as_sf()

# Sanity: ensure no NA in chosen vars
stopifnot(all(colSums(is.na(final_sf %>% st_drop_geometry() %>% select(all_of(chosen)))) == 0))

# 8) Save clean model-ready dataset
out_rds <- file.path(OUT_CLEAN, paste0("CA_", MY_CA_NUM, "_clean.rds"))
write_rds(final_sf, out_rds)
final_sf %>% st_drop_geometry() %>%
  write_csv(file.path(OUT_CLEAN, paste0("CA_", MY_CA_NUM, "_clean_no_geom.csv")))
message("✅ Saved model-ready dataset (complete cases only): ", out_rds)

# 9) quick EDA plots of chosen set
long_df <- final_sf %>% st_drop_geometry() %>%
  select(all_of(chosen)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

p_hist <- ggplot(long_df, aes(x = value)) +
  geom_histogram(bins = 30, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = paste0("CA ", MY_CA_NUM, " + neighbors — Distributions of chosen variables (", ACS_YEAR, ")"),
       x = NULL, y = "Count") +
  theme_minimal()

ggsave(file.path(OUT_EXP, paste0("chosen_vars_hist_CA_", MY_CA_NUM, ".png")),
       p_hist, width = 9, height = 6, dpi = 150)

chosen_cor <- final_sf %>% st_drop_geometry() %>% select(all_of(chosen)) %>%
  cor(use = "pairwise.complete.obs")
cor_df <- melt(chosen_cor, varnames = c("x","y"), value.name = "r")

p_cor <- ggplot(cor_df, aes(x, y, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1,1)) +
  labs(title = "Correlation of chosen variables", x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_EXP, paste0("chosen_vars_cor_CA_", MY_CA_NUM, ".png")),
       p_cor, width = 6.5, height = 5.5, dpi = 150)

message("Saved EDA plots to ", OUT_EXP)