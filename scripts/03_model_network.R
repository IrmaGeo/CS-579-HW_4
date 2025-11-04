# 03_model_network.R

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
})
# 0) Parameters

MY_CA_NUM   <- "30"
MY_CA_NAME  <- "SOUTH LAWNDALE"

CLEAN_DIR     <- "data/clean_data"
OUT_DIR       <- "data/model_outputs"
CA_SHAPE_PATH <- "/Users/irmamodzgvrishvili/Desktop/Education/Illinois/Fall25/CS579/Assignement_4/Boundaries - Community Areas_20251024"

K_NEIGHBORS <- 5
set.seed(579)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 1) Load CA boundaries
chi_ca_sf <- st_read(CA_SHAPE_PATH, quiet = TRUE) |>
  st_transform(4326) |>
  select(area_numbe, community) |>
  mutate(area_numbe = as.character(area_numbe))

# 2) Read clean datasets
clean_files <- list.files(CLEAN_DIR, pattern = "^CA_.*_clean\\.rds$", full.names = TRUE)
if (length(clean_files) == 0) {
  stop("No clean datasets found in ", CLEAN_DIR, ". Run 02_pull_acs_eda_select.R for each teammate.")
}
cat("Found", length(clean_files), "clean files:\n")
print(basename(clean_files))

sflist <- lapply(clean_files, readr::read_rds)
req_cols <- c("GEOID", "CA_Number", "CA_Name", "geometry")
sflist <- sflist[map_lgl(sflist, ~ inherits(., "sf") && all(req_cols %in% names(.)))]
if (length(sflist) == 0) stop("No valid sf datasets with required columns.")

# 3) Common scaled features
get_scaled_cols <- function(x) names(x)[str_ends(names(x), "_Z")]
scaled_cols_list <- lapply(sflist, get_scaled_cols)
common_scaled <- Reduce(intersect, scaled_cols_list)
if (length(common_scaled) < 4) {
  warning("Fewer than 4 common scaled features across teammates. Falling back to any scaled columns present in the first file.")
  common_scaled <- scaled_cols_list[[1]]
}
cat("Using", length(common_scaled), "common scaled features:\n")
print(common_scaled)

# 4) Bind, dedupe, complete cases
all_sf <- dplyr::bind_rows(lapply(sflist, function(s) {
  s |>
    dplyr::select(dplyr::any_of(c(req_cols, common_scaled))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(common_scaled), as.numeric))
}))

all_sf <- all_sf |>
  dplyr::mutate(.na_cnt = rowSums(dplyr::across(dplyr::all_of(common_scaled), ~ is.na(.)))) |>
  dplyr::arrange(.na_cnt) |>
  dplyr::group_by(GEOID) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(-.na_cnt)

complete_sf <- all_sf |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(common_scaled), ~ !is.na(.)))

dropped_geoids <- setdiff(all_sf$GEOID, complete_sf$GEOID)
if (length(dropped_geoids) > 0) {
  readr::write_lines(dropped_geoids, file.path(OUT_DIR, "dropped_geoids_due_to_NA.txt"))
  message("Dropped ", length(dropped_geoids), " BGs due to NA in common features (logged).")
}
cat("Final BG count for modeling:", nrow(complete_sf), "\n")

# 5) Build kNN similarity network
X <- complete_sf |> st_drop_geometry() |> select(all_of(common_scaled)) |> as.matrix()
dist_mat <- as.matrix(dist(X, method = "euclidean"))
diag(dist_mat) <- Inf

geoids <- complete_sf$GEOID
adj_list <- lapply(seq_len(nrow(dist_mat)), function(i) {
  nbr_idx <- order(dist_mat[i, ])[1:K_NEIGHBORS]
  data.frame(from = geoids[i], to = geoids[nbr_idx], stringsAsFactors = FALSE)
})
edge_list <- bind_rows(adj_list) |>
  mutate(u = pmin(from, to), v = pmax(from, to)) |>
  distinct(u, v, .keep_all = FALSE) |>
  rename(from = u, to = v)

# 6) Graph, centrality, Louvain
vertex_df <- complete_sf |>
  st_drop_geometry() |>
  select(GEOID, CA_Number, CA_Name)

bg_graph <- graph_from_data_frame(d = edge_list, vertices = vertex_df, directed = FALSE)

V(bg_graph)$degree      <- degree(bg_graph)
V(bg_graph)$betweenness <- betweenness(bg_graph, directed = FALSE, normalized = TRUE)
V(bg_graph)$closeness   <- closeness(bg_graph, normalized = TRUE)

comm_louvain <- cluster_louvain(bg_graph)
V(bg_graph)$louvain_community <- membership(comm_louvain)
modularity_score <- modularity(comm_louvain)

cat("Graph summary — Nodes:", vcount(bg_graph), "Edges:", ecount(bg_graph),
    "Modularity:", round(modularity_score, 3), "\n")

# Join graph attributes back to sf
graph_attributes <- tibble(
  GEOID = igraph::V(bg_graph)$name,
  degree = igraph::V(bg_graph)$degree,
  betweenness = igraph::V(bg_graph)$betweenness,
  closeness = igraph::V(bg_graph)$closeness,
  louvain_community = igraph::V(bg_graph)$louvain_community
)
bg_results <- complete_sf |> left_join(graph_attributes, by = "GEOID")

# Top bridges table
top_bridges <- bg_results |>
  st_drop_geometry() |>
  arrange(desc(betweenness)) |>
  select(GEOID, CA_Number, betweenness) |>
  head(10)
write_csv(top_bridges, file.path(OUT_DIR, "top_bridges.csv"))
print(top_bridges)

# 6.1 Plots (Louvain map & network)
included_cas <- unique(bg_results$CA_Number)
base_map <- chi_ca_sf |> filter(area_numbe %in% included_cas)
focus_outline     <- base_map |> filter(area_numbe == MY_CA_NUM)
neighbors_outline <- base_map |> filter(area_numbe != MY_CA_NUM)

# Plot 1: Louvain communities vs CA boundaries
p_map <- ggplot() +
  geom_sf(data = neighbors_outline, fill = "grey95", color = "grey80", linewidth = 0.3) +
  geom_sf(data = bg_results, aes(fill = factor(louvain_community)), color = "white", linewidth = 0.1, alpha = 0.95) +
  geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 1) +
  scale_fill_brewer(palette = "Set3", name = "Louvain cluster") +
  labs(
    title = paste("Alternative Communities (Louvain) — CA", MY_CA_NUM, MY_CA_NAME),
    subtitle = "Colors = data-driven clusters; Black outline = focal CA boundary",
    caption = "kNN (k=5) on standardized ACS features"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", plot.title = element_text(face = "bold")) +
  coord_sf()
ggsave(file.path(OUT_DIR, paste0("plot_louvain_CA_", MY_CA_NUM, ".png")),
       p_map, width = 9, height = 7, dpi = 150)

# Plot 2: Similarity network & gatekeepers
tmp_cent  <- st_centroid(bg_results)
coords    <- st_coordinates(tmp_cent)
bg_centroids <- tmp_cent |>
  st_drop_geometry() |>
  mutate(X = coords[,1], Y = coords[,2]) |>
  select(GEOID, X, Y)

edge_coords <- edge_list |>
  left_join(bg_centroids, by = c("from" = "GEOID")) |>
  rename(from_x = X, from_y = Y) |>
  left_join(bg_centroids, by = c("to" = "GEOID")) |>
  rename(to_x = X, to_y = Y) |>
  filter(!is.na(from_x), !is.na(to_x))

btw_scaled <- rescale(bg_results$betweenness, to = c(1, 12),
                      from = range(bg_results$betweenness, na.rm = TRUE))
bg_results$btw_size <- sqrt(btw_scaled)

p_net <- ggplot() +
  geom_sf(data = neighbors_outline, fill = "grey98", color = "grey90", linewidth = 0.2) +
  geom_sf(data = focus_outline, fill = NA, color = "black", linewidth = 0.9) +
  geom_segment(data = edge_coords,
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               alpha = 0.12, linewidth = 0.35) +
  geom_sf(data = bg_results, aes(size = btw_size, color = CA_Number),
          alpha = 0.85, show.legend = "point") +
  scale_size_continuous(name = "Betweenness (relative)", range = c(1.5, 7)) +
  scale_color_brewer(palette = "Dark2", name = "Community Area") +
  labs(
    title = paste("Similarity Network & Gatekeepers — CA", MY_CA_NUM, MY_CA_NAME),
    subtitle = "Edges: kNN similarity; Node size: betweenness centrality"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", plot.title = element_text(face = "bold")) +
  coord_sf()
ggsave(file.path(OUT_DIR, paste0("plot_network_gatekeepers_CA_", MY_CA_NUM, ".png")),
       p_net, width = 9, height = 7, dpi = 150)

# 7) “Is” vs “Not” community metrics
vattrs <- data.frame(
  GEOID = igraph::V(bg_graph)$name,
  CA    = igraph::V(bg_graph)$CA_Number,
  stringsAsFactors = FALSE
)
edges2 <- igraph::as_data_frame(bg_graph, what = "edges") %>%
  dplyr::left_join(vattrs, by = c("from" = "GEOID")) %>%
  dplyr::rename(CA_from = CA) %>%
  dplyr::left_join(vattrs, by = c("to" = "GEOID")) %>%
  dplyr::rename(CA_to = CA)

num_edges_total  <- nrow(edges2)
num_edges_within <- sum(edges2$CA_from == MY_CA_NUM & edges2$CA_to == MY_CA_NUM, na.rm = TRUE)
within_ca <- ifelse(num_edges_total > 0, num_edges_within / num_edges_total, NA_real_)
assort <- igraph::assortativity_nominal(
  bg_graph, as.integer(factor(igraph::V(bg_graph)$CA_Number)), directed = FALSE
)

S_mask    <- (igraph::V(bg_graph)$CA_Number == MY_CA_NUM)
cut_edges <- sum( (edges2$CA_from == MY_CA_NUM) != (edges2$CA_to == MY_CA_NUM), na.rm = TRUE )
deg_vec   <- igraph::degree(bg_graph)
vol_S     <- sum(deg_vec[S_mask])
vol_notS  <- sum(deg_vec[!S_mask])
conductance_ca <- ifelse(min(vol_S, vol_notS) > 0, cut_edges / min(vol_S, vol_notS), NA_real_)

metrics <- tibble(
  nodes = igraph::vcount(bg_graph),
  edges = igraph::ecount(bg_graph),
  modularity = modularity_score,
  within_CA_edge_fraction = within_ca,
  assortativity_by_CA = assort,
  conductance_focal_CA = conductance_ca,
  focal_CA = MY_CA_NUM
)
readr::write_csv(metrics, file.path(OUT_DIR, paste0("network_metrics_CA_", MY_CA_NUM, ".csv")))
print(metrics)

cat("\n--- Narrative bullets for report ---\n")
cat("* Modularity (Louvain):", round(modularity_score, 3), "\n")
cat("* Within-CA edge fraction (CA ", MY_CA_NUM, "): ", round(within_ca, 3), "\n", sep = "")
cat("* Assortativity by CA: ", round(assort, 3), "\n", sep = "")
cat("* Conductance (CA ", MY_CA_NUM, "): ", round(conductance_ca, 3), " (lower = tighter)\n", sep = "")
cat("* See top_bridges.csv for high-betweenness 'bridge' block groups.\n")

# 8) Report add-ons
inc_summary <- bg_results |>
  sf::st_drop_geometry() |>
  dplyr::count(CA_Number, name = "BG_Count") |>
  dplyr::arrange(desc(BG_Count))
print(inc_summary)

cluster_sizes <- bg_results |>
  st_drop_geometry() |>
  count(louvain_community, name = "n_BG") |>
  arrange(desc(n_BG))
write_csv(cluster_sizes, file.path(OUT_DIR, "cluster_sizes_overall.csv"))

cluster_sizes_ca <- bg_results |>
  st_drop_geometry() |>
  filter(CA_Number == MY_CA_NUM) |>
  count(louvain_community, name = "n_BG_in_focal_CA") |>
  arrange(desc(n_BG_in_focal_CA))
write_csv(cluster_sizes_ca, file.path(OUT_DIR, paste0("cluster_sizes_CA_", MY_CA_NUM, ".csv")))

cluster_mix <- bg_results |>
  st_drop_geometry() |>
  group_by(louvain_community, CA_Number) |>
  summarise(n_BG = dplyr::n(), .groups = "drop_last") |>
  mutate(share = n_BG / sum(n_BG)) |>
  arrange(louvain_community, desc(share)) |>
  ungroup()
write_csv(cluster_mix, file.path(OUT_DIR, "cluster_CA_mix.csv"))

# 9) Alternative communities (GeoPackage)
alt_communities_ca <- bg_results |>
  filter(CA_Number == MY_CA_NUM) |>
  select(GEOID, louvain_community, geometry)

alt_path <- file.path(OUT_DIR, paste0("CA", MY_CA_NUM, "_alternative_communities.gpkg"))
if (file.exists(alt_path)) {
  try(unlink(alt_path), silent = TRUE)
}
st_write(alt_communities_ca, alt_path,
         layer = paste0("CA", MY_CA_NUM, "_Louvain"), delete_dsn = TRUE)
message("Saved alternative communities GeoPackage: ", alt_path)

# 10) k-sensitivity
sweep_k <- function(k, X, geoids, vertex_df) {
  D <- as.matrix(dist(X)); diag(D) <- Inf
  edges <- do.call(rbind, lapply(seq_len(nrow(D)), function(i){
    j <- order(D[i, ])[1:k]; data.frame(from = geoids[i], to = geoids[j])
  })) |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    distinct(u, v) |>
    rename(from = u, to = v)

  g  <- igraph::graph_from_data_frame(edges, vertices = vertex_df, directed = FALSE)
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

X_mat <- complete_sf |> st_drop_geometry() |> select(all_of(common_scaled)) |> as.matrix()
geo    <- complete_sf$GEOID
vtx    <- complete_sf |> st_drop_geometry() |> select(GEOID, CA_Number, CA_Name)

k_values  <- 4:8
k_results <- base::do.call(rbind, base::lapply(k_values, function(k) {
  sweep_k(k, X = X_mat, geoids = geo, vertex_df = vtx)
}))

readr::write_csv(k_results, file.path(OUT_DIR, "k_sensitivity.csv"))
print(k_results)