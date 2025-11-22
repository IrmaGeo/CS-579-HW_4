# CS579 Final Project — 05_network_model.R
# Multi-layer network for Chicago:
#   Layer A1: CA adjacency (shared borders)
#   Layer A2: BG adjacency (shared borders)
#   Layer B : BG similarity (k-NN on standardized ACS features)

options(tigris_use_cache = TRUE, scipen = 999)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(igraph)
  library(readr)
  library(stringr)
  library(purrr)
  library(scales)
  library(tibble)
  library(ggrepel)
  library(viridis)   # nice unlimited palettes
})

sf_use_s2(TRUE)

# 0) Parameters -------------------------------------------------------------

MY_CA_NUM <- "30"

CLEAN_FILE <- file.path("data", "clean_data", "CA_ALL_clean.rds")

METRICS_DIR <- file.path("data", "network_outputs")
FIG_DIR     <- file.path("results", "figures", "network_layers")

# keep your local path for now; can later move shapefile into data/raw/
CA_SHAPE_PATH <- "/Users/irmamodzgvrishvili/Desktop/Education/Illinois/Fall25/CS579/Assignement_4/Boundaries - Community Areas_20251024"

K_NEIGHBORS <- 5
set.seed(579)

dir.create(METRICS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,     showWarnings = FALSE, recursive = TRUE)

# 1) Load CA boundaries + cleaned BG data ----------------------------------

chi_ca_sf <- st_read(CA_SHAPE_PATH, quiet = TRUE) |>
  st_transform(4326) |>
  select(area_numbe, community) |>
  mutate(area_numbe = as.character(area_numbe))

if (!file.exists(CLEAN_FILE)) {
  stop(
    "Clean city-wide BG file not found: ", CLEAN_FILE,
    "\nRun 02_clean_select_variables.R first."
  )
}

bg_sf <- readr::read_rds(CLEAN_FILE)
stopifnot(inherits(bg_sf, "sf"))

req_cols <- c("GEOID", "CA_Number", "CA_Name", "geometry")
if (!all(req_cols %in% names(bg_sf))) {
  stop(
    "bg_sf is missing required columns: ",
    paste(setdiff(req_cols, names(bg_sf)), collapse = ", ")
  )
}

# standardized features for similarity
scaled_cols <- names(bg_sf)[stringr::str_ends(names(bg_sf), "_Z")]
if (length(scaled_cols) < 4) {
  stop(
    "Expected at least 4 scaled variables (ending with _Z). Found: ",
    paste(scaled_cols, collapse = ", ")
  )
}

# one row per BG
bg_sf <- bg_sf |>
  st_as_sf() |>
  dplyr::group_by(GEOID) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

message("Final BG count (city-wide): ", nrow(bg_sf))
included_cas <- sort(unique(bg_sf$CA_Number))

# 2) Layer B: BG similarity (kNN) ------------------------------------------

message("\n--- Building Layer B: BG similarity (kNN, k = ", K_NEIGHBORS, ") ---")

X <- bg_sf |>
  st_drop_geometry() |>
  select(all_of(scaled_cols)) |>
  as.matrix()

dist_mat <- as.matrix(dist(X, method = "euclidean"))
diag(dist_mat) <- Inf

geoids <- bg_sf$GEOID
adj_list_B <- lapply(seq_len(nrow(dist_mat)), function(i) {
  nbr_idx <- order(dist_mat[i, ])[1:K_NEIGHBORS]
  data.frame(from = geoids[i], to = geoids[nbr_idx], stringsAsFactors = FALSE)
})

edge_list_B <- bind_rows(adj_list_B) |>
  mutate(u = pmin(from, to), v = pmax(from, to)) |>
  distinct(u, v, .keep_all = FALSE) |>
  rename(from = u, to = v)

vertex_df_B <- bg_sf |>
  st_drop_geometry() |>
  select(GEOID, CA_Number, CA_Name)

g_B <- graph_from_data_frame(
  d = edge_list_B,
  vertices = vertex_df_B |> rename(name = GEOID),
  directed = FALSE
)

V(g_B)$degree_B      <- degree(g_B)
V(g_B)$betweenness_B <- betweenness(g_B, directed = FALSE, normalized = TRUE)
V(g_B)$closeness_B   <- closeness(g_B, normalized = TRUE)

comm_B           <- cluster_louvain(g_B)
V(g_B)$louvain_B <- membership(comm_B)
modularity_B     <- modularity(comm_B)

message(
  "Layer B (BG similarity) — Nodes: ", vcount(g_B),
  " Edges: ", ecount(g_B),
  " Modularity: ", round(modularity_B, 3)
)

graph_attr_B <- tibble(
  GEOID         = igraph::V(g_B)$name,
  degree_B      = igraph::V(g_B)$degree_B,
  betweenness_B = igraph::V(g_B)$betweenness_B,
  closeness_B   = igraph::V(g_B)$closeness_B,
  louvain_B     = igraph::V(g_B)$louvain_B
)

bg_results <- bg_sf |>
  left_join(graph_attr_B, by = "GEOID")

top_bridges_B <- bg_results |>
  st_drop_geometry() |>
  arrange(desc(betweenness_B)) |>
  select(GEOID, CA_Number, CA_Name, betweenness_B) |>
  head(20)

write_csv(
  top_bridges_B,
  file.path(METRICS_DIR, "layerB_top_bridges_citywide.csv")
)

# 3) Layer A1: CA adjacency -------------------------------------------------

message("\n--- Building Layer A1: CA adjacency network ---")

ca_sub <- chi_ca_sf |>
  filter(area_numbe %in% included_cas)

touch_list_CA <- st_touches(ca_sub, ca_sub)

edge_list_A_CA <- purrr::map2_dfr(
  seq_along(touch_list_CA), touch_list_CA,
  function(i, nbrs) {
    if (length(nbrs) == 0) return(NULL)
    from_id <- ca_sub$area_numbe[i]
    data.frame(
      from = from_id,
      to   = ca_sub$area_numbe[nbrs],
      stringsAsFactors = FALSE
    )
  }
) |>
  mutate(u = pmin(from, to), v = pmax(from, to)) |>
  distinct(u, v, .keep_all = FALSE) |>
  rename(from = u, to = v)

vertex_df_A_CA <- ca_sub |>
  st_drop_geometry() |>
  transmute(
    CA_ID   = area_numbe,
    CA_Name = community
  )

g_A_CA <- graph_from_data_frame(
  d = edge_list_A_CA,
  vertices = vertex_df_A_CA |> rename(name = CA_ID),
  directed = FALSE
)

V(g_A_CA)$degree_A_CA <- degree(g_A_CA)
V(g_A_CA)$btw_A_CA    <- betweenness(g_A_CA, directed = FALSE, normalized = TRUE)
V(g_A_CA)$close_A_CA  <- closeness(g_A_CA, normalized = TRUE)

comm_A_CA              <- cluster_louvain(g_A_CA)
V(g_A_CA)$louvain_A_CA <- membership(comm_A_CA)
modularity_A_CA        <- modularity(comm_A_CA)

ca_attr_A_CA <- tibble(
  area_numbe       = igraph::V(g_A_CA)$name,
  degree_A_CA      = igraph::V(g_A_CA)$degree_A_CA,
  betweenness_A_CA = igraph::V(g_A_CA)$btw_A_CA,
  closeness_A_CA   = igraph::V(g_A_CA)$close_A_CA,
  louvain_A_CA     = igraph::V(g_A_CA)$louvain_A_CA
)

ca_results_CA <- ca_sub |>
  left_join(ca_attr_A_CA, by = "area_numbe")

# 4) Layer A2: BG adjacency -------------------------------------------------

message("\n--- Building Layer A2: BG adjacency (shared borders) ---")

touch_list_BG <- st_touches(bg_sf, bg_sf)

edge_list_A_BG <- purrr::map2_dfr(
  seq_along(touch_list_BG), touch_list_BG,
  function(i, nbrs) {
    if (length(nbrs) == 0) return(NULL)
    from_id <- bg_sf$GEOID[i]
    data.frame(
      from = from_id,
      to   = bg_sf$GEOID[nbrs],
      stringsAsFactors = FALSE
    )
  }
) |>
  mutate(u = pmin(from, to), v = pmax(from, to)) |>
  distinct(u, v, .keep_all = FALSE) |>
  rename(from = u, to = v)

vertex_df_A_BG <- bg_sf |>
  st_drop_geometry() |>
  select(GEOID, CA_Number, CA_Name)

g_A_BG <- graph_from_data_frame(
  d = edge_list_A_BG,
  vertices = vertex_df_A_BG |> rename(name = GEOID),
  directed = FALSE
)

V(g_A_BG)$degree_A_BG <- degree(g_A_BG)
V(g_A_BG)$btw_A_BG    <- betweenness(g_A_BG, directed = FALSE, normalized = TRUE)
V(g_A_BG)$close_A_BG  <- closeness(g_A_BG, normalized = TRUE)

comm_A_BG              <- cluster_louvain(g_A_BG)
V(g_A_BG)$louvain_A_BG <- membership(comm_A_BG)
modularity_A_BG        <- modularity(comm_A_BG)

bg_attr_A_BG <- tibble(
  GEOID            = igraph::V(g_A_BG)$name,
  degree_A_BG      = igraph::V(g_A_BG)$degree_A_BG,
  betweenness_A_BG = igraph::V(g_A_BG)$btw_A_BG,
  closeness_A_BG   = igraph::V(g_A_BG)$close_A_BG,
  louvain_A_BG     = igraph::V(g_A_BG)$louvain_A_BG
)

bg_results <- bg_results |>
  left_join(bg_attr_A_BG, by = "GEOID")

# 5) Metrics for BG-based layers -------------------------------------------

compute_bg_layer_metrics <- function(g, layer_label, MY_CA_NUM) {
  vattrs <- data.frame(
    GEOID = igraph::V(g)$name,
    CA    = igraph::V(g)$CA_Number,
    stringsAsFactors = FALSE
  )
  edges2 <- igraph::as_data_frame(g, what = "edges") |>
    dplyr::left_join(vattrs, by = c("from" = "GEOID")) |>
    dplyr::rename(CA_from = CA) |>
    dplyr::left_join(vattrs, by = c("to" = "GEOID")) |>
    dplyr::rename(CA_to = CA)

  num_edges_total  <- nrow(edges2)
  num_edges_within <- sum(edges2$CA_from == edges2$CA_to, na.rm = TRUE)
  within_same_CA_frac <- ifelse(
    num_edges_total > 0,
    num_edges_within / num_edges_total,
    NA_real_
  )

  assort_CA <- igraph::assortativity_nominal(
    g, as.integer(factor(igraph::V(g)$CA_Number)), directed = FALSE
  )

  S_mask <- (igraph::V(g)$CA_Number == MY_CA_NUM)
  cut_edges <- sum(
    (edges2$CA_from == MY_CA_NUM) != (edges2$CA_to == MY_CA_NUM),
    na.rm = TRUE
  )
  deg_vec <- igraph::degree(g)
  vol_S    <- sum(deg_vec[S_mask])
  vol_notS <- sum(deg_vec[!S_mask])
  conductance_CA <- ifelse(
    min(vol_S, vol_notS) > 0,
    cut_edges / min(vol_S, vol_notS),
    NA_real_
  )

  modularity_val <- modularity(cluster_louvain(g))

  tibble(
    layer                    = layer_label,
    nodes                    = igraph::vcount(g),
    edges                    = igraph::ecount(g),
    modularity               = modularity_val,
    within_same_CA_edge_frac = within_same_CA_frac,
    assortativity_by_CA      = assort_CA,
    conductance_focal_CA     = conductance_CA,
    focal_CA                 = MY_CA_NUM
  )
}

metrics_B  <- compute_bg_layer_metrics(g_B,    "BG_similarity_kNN", MY_CA_NUM)
metrics_A2 <- compute_bg_layer_metrics(g_A_BG, "BG_adjacency",      MY_CA_NUM)

write_csv(
  metrics_B,
  file.path(METRICS_DIR, "layerB_network_metrics_citywide.csv")
)
write_csv(
  metrics_A2,
  file.path(METRICS_DIR, "layerA2_BGadj_network_metrics_citywide.csv")
)

# CA adjacency metrics (no within-CA at CA level)
metrics_A1 <- tibble(
  layer                    = "CA_adjacency",
  nodes                    = igraph::vcount(g_A_CA),
  edges                    = igraph::ecount(g_A_CA),
  modularity               = modularity_A_CA,
  within_same_CA_edge_frac = NA_real_,
  assortativity_by_CA      = NA_real_,
  conductance_focal_CA     = NA_real_,
  focal_CA                 = MY_CA_NUM
)

write_csv(
  metrics_A1,
  file.path(METRICS_DIR, "layerA1_CAadj_network_metrics_citywide.csv")
)

# 6) Plots: Layer B (clusters + network) -----------------------------------

base_map <- chi_ca_sf |>
  filter(area_numbe %in% included_cas)

# ---- Layer B: Louvain clusters ----
p_louvain_B <- ggplot() +
  geom_sf(data = base_map,
          fill = "grey95", color = "grey70", linewidth = 0.4) +
  geom_sf(data = bg_results,
          aes(fill = factor(louvain_B)),
          color = NA, alpha = 0.9) +
  scale_fill_viridis_d(option = "turbo", name = "Louvain cluster\n(Layer B)") +
  labs(
    title    = "Layer B: Block-group similarity clusters",
    subtitle = "k-NN on standardized ACS variables (city-wide)",
    caption  = "CS579 Final Project — Irma Modzgvrishvili"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(size = 11),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_sf()

ggsave(
  file.path(FIG_DIR, "layerB_louvain_clusters_citywide.png"),
  p_louvain_B, width = 9, height = 6.5, dpi = 200
)

# ---- Layer B: network & gatekeepers ----
tmp_cent  <- st_centroid(bg_results)
coords    <- st_coordinates(tmp_cent)
bg_centroids <- tmp_cent |>
  st_drop_geometry() |>
  mutate(X = coords[,1], Y = coords[,2]) |>
  select(GEOID, X, Y)

edge_coords_B <- edge_list_B |>
  left_join(bg_centroids, by = c("from" = "GEOID")) |>
  rename(from_x = X, from_y = Y) |>
  left_join(bg_centroids, by = c("to" = "GEOID")) |>
  rename(to_x = X, to_y = Y) |>
  filter(!is.na(from_x), !is.na(to_x))

btw_scaled_B <- rescale(
  bg_results$betweenness_B,
  to   = c(1, 12),
  from = range(bg_results$betweenness_B, na.rm = TRUE)
)
bg_results$btw_size_B <- sqrt(btw_scaled_B)

p_net_B <- ggplot() +
  geom_sf(data = base_map,
          fill = "grey98", color = "grey80", linewidth = 0.3) +
  geom_segment(
    data = edge_coords_B,
    aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
    alpha = 0.08, linewidth = 0.25
  ) +
  geom_sf(
    data = bg_results,
    aes(size = btw_size_B, color = CA_Number),
    alpha = 0.85, show.legend = "point"
  ) +
  scale_size_continuous(
    name  = "Betweenness\n(Layer B, relative)",
    range = c(1.5, 6)
  ) +
  scale_color_viridis_d(option = "plasma", name = "Community Area") +
  labs(
    title    = "Layer B: Similarity network & gatekeeper BGs",
    subtitle = "Edges: k-NN similarity on ACS features; Node size: betweenness",
    caption  = "BG centroids; CA boundaries in grey"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(size = 11),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  coord_sf()

ggsave(
  file.path(FIG_DIR, "layerB_network_gatekeepers_citywide.png"),
  p_net_B, width = 9, height = 6.5, dpi = 200
)

# 7) Plots: Layer A1 (CA adjacency) ----------------------------------------

p_louvain_A_CA <- ggplot() +
  geom_sf(data = chi_ca_sf,
          fill = "grey95", color = "white", linewidth = 0.3) +
  geom_sf(data = ca_results_CA,
          aes(fill = factor(louvain_A_CA)),
          color = "grey20", linewidth = 0.4) +
  scale_fill_viridis_d(option = "magma", name = "Louvain cluster\n(Layer A1)") +
  labs(
    title    = "Layer A1: Community-area adjacency clusters",
    subtitle = "Communities based on shared CA borders",
    caption  = "CS579 Final Project — Irma Modzgvrishvili"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(size = 11),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_sf()

ggsave(
  file.path(FIG_DIR, "layerA1_CA_adjacency_clusters.png"),
  p_louvain_A_CA, width = 8.5, height = 6.5, dpi = 200
)

# 8) Plots: Layer A2 (BG adjacency) ----------------------------------------

p_louvain_A_BG <- ggplot() +
  geom_sf(data = base_map,
          fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = bg_results,
          aes(fill = factor(louvain_A_BG)),
          color = NA, alpha = 0.9) +
  scale_fill_viridis_d(option = "cividis", name = "Louvain cluster\n(Layer A2)") +
  labs(
    title    = "Layer A2: Block-group adjacency clusters",
    subtitle = "Communities based on shared block-group borders",
    caption  = "CS579 Final Project — Irma Modzgvrishvili"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(size = 11),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_sf()

ggsave(
  file.path(FIG_DIR, "layerA2_BG_adjacency_clusters.png"),
  p_louvain_A_BG, width = 9, height = 6.5, dpi = 200
)

# 9) k-sensitivity for Layer B ---------------------------------------------

sweep_k <- function(k, X, geoids, vertex_df) {
  D <- as.matrix(dist(X)); diag(D) <- Inf

  edges <- do.call(rbind, lapply(seq_len(nrow(D)), function(i) {
    j <- order(D[i, ])[1:k]
    data.frame(from = geoids[i], to = geoids[j])
  })) |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    distinct(u, v) |>
    rename(from = u, to = v)

  g  <- igraph::graph_from_data_frame(
    edges,
    vertices = vertex_df |> rename(name = GEOID),
    directed = FALSE
  )
  cl <- igraph::cluster_louvain(g)

  data.frame(
    k          = k,
    nodes      = igraph::vcount(g),
    edges      = igraph::ecount(g),
    modularity = igraph::modularity(cl),
    assort     = igraph::assortativity_nominal(
      g, as.integer(factor(igraph::V(g)$CA_Number)), directed = FALSE
    )
  )
}

X_mat <- bg_sf |>
  st_drop_geometry() |>
  select(all_of(scaled_cols)) |>
  as.matrix()

geo_vec <- bg_sf$GEOID
vtx_B   <- bg_sf |>
  st_drop_geometry() |>
  select(GEOID, CA_Number, CA_Name)

k_values  <- 4:8
k_results <- do.call(rbind, lapply(k_values, function(k) {
  sweep_k(k, X = X_mat, geoids = geo_vec, vertex_df = vtx_B)
}))

write_csv(
  k_results,
  file.path(METRICS_DIR, "layerB_k_sensitivity_citywide.csv")
)

message(
  "\n✅ Multi-layer network modeling complete.\n",
  "   Metrics in: ", METRICS_DIR, "\n",
  "   Figures in: ", FIG_DIR
)