# Comment out below if they are not installed:
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("tidycensus")
# install.packages("curl", type = "source")
# install.packages("tigris")

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)
library(tidycensus)
library(tigris)

getwd()


# Data is downloaded from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
# Download steps:
#     1. Select community
#     2. Click "Export" button
#     3. export as shapefile.
chi_ca_file <- here::here("Boundaries_CommunityAreas_20251004/geo_export_2f67acac-76d9-47a1-9c12-fcf8534da850.shp")

file.exists(chi_ca_file)

chi_ca_sf <- sf::st_read(chi_ca_file)
nrow(chi_ca_sf)
head(chi_ca_sf)

st_crs(chi_ca_sf)

# suggest_crs(chi_ca_sf)

chi_ca_proj_sf <- sf::st_transform(chi_ca_sf, crs=4269)
nrow(chi_ca_proj_sf)


head(chi_ca_proj_sf)

ggplot(chi_ca_proj_sf) + 
    geom_sf() + 
    labs(title = "Chicago Community Areas NAD83")

# community_names <- c("WEST GARFIELD PARK", "EAST GARFIELD PARK")
community_names <- c("EAST GARFIELD PARK")
community_names_str <- paste(community_names, collapse = ", ")

community_sf <- dplyr::filter(chi_ca_proj_sf, community %in% community_names)
nrow(community_sf)

head(community_sf)

ggplot(community_sf) +
    geom_sf() +
    labs(title = paste("Community Areas:", community_names_str))

total_population_10 <- get_decennial(
    geography = "state",
    variables = "P001001",
    year = 2010
)

st <- states()
nrow(st)

head(st)

ggplot(st) +
 geom_sf() +
 labs(title = "States")

il_counties <- counties("IL", year=2024)
nrow(il_counties)

head(il_counties)

ggplot(il_counties) +
    geom_sf()+
    labs(title = "Illinois Counties")

cook_county <- dplyr::filter(il_counties, NAME == "Cook")
nrow(cook_county)

head(cook_county)

ggplot(cook_county) +
    geom_sf() +
    labs(title="Cook County")

inherits(cook_county, "sf")

ggplot(cook_county) +
    geom_sf() +
    geom_sf(data=chi_ca_proj_sf)+
    labs(title="Cook County and Community Areas")

cook_tracts <- tracts("IL", "Cook")
nrow(cook_tracts)

head(cook_tracts)

ggplot() +
    geom_sf(data = cook_county)+
    geom_sf(data = cook_tracts)+
    labs(title = "Cook County Census Tracts")

cook_bgs <- block_groups("IL", "Cook", year=2024)
nrow(cook_tracts)

head(cook_tracts)

ggplot() +
    geom_sf(data = cook_county)+
    geom_sf(data = cook_tracts)+
    geom_sf(data = cook_bgs)+
    labs(title = "Cook County Census Tracts and Block Groups")

cook_blocks <- blocks("IL", "Cook", year=2024)
nrow(cook_blocks)

head(cook_blocks)


ggplot() +
    geom_sf(data = cook_county)+
    geom_sf(data = cook_tracts)+
    geom_sf(data = cook_bgs)+
    geom_sf(data = cook_blocks)+
    labs(title = "Cook County Census Tracts, Block Groups & Blocks")


# intersect_bgs <- sf::st_join(
#   cook_bgs,
#   community_sf,
#   join = st_within,  # or st_intersects if you want partial overlaps too
#   # join = st_intersects,
#   left = FALSE
# )

# Logical vector: TRUE if cook_bgs is fully within any community polygon
# within_logical <- lengths(st_within(cook_bgs, community_sf)) > 0

bgs_intersect_community <- lengths(st_intersects(cook_bgs, community_sf)) > 0

# Subset cook_bgs to get only those fully inside communities
intersect_bgs <- cook_bgs[bgs_intersect_community, ]
nrow(intersect_bgs)
head(intersect_bgs)

ggplot() +
    geom_sf(data = community_sf, fill = "lightblue") +
    geom_sf(data = intersect_bgs, color = "red", fill = NA) +
    labs(title = paste("Block Groups within Community:", community_names_str))

# neighbor_bgs <- st_join(
#   cook_bgs,
#   intersect_bgs,
#   join = st_intersects,
#   left = FALSE  # only keep matches
# )

bgs_neighboring_community <- lengths(st_intersects(cook_bgs, intersect_bgs)) > 0
neighbor_bgs <- cook_bgs[bgs_neighboring_community, ]
nrow(neighbor_bgs)

head(neighbor_bgs)

ggplot() +
    geom_sf(data = community_sf, fill = "lightblue") +
    geom_sf(data = neighbor_bgs, color = "red", fill = NA) +
    labs(title = paste("Block Groups within Community:", community_names_str))

colnames(neighbor_bgs)
colnames(cook_bgs)

st_write(neighbor_bgs, "report/neighbor_bgs.shp", delete_layer = TRUE)

# Project to a planar CRS (e.g., NAD83 / Illinois StatePlane East in meters)
neighbor_bgs_proj <- st_transform(neighbor_bgs, crs = 3435)
community_sf_proj <- st_transform(community_sf, crs = 3435)

# Compute centroids in projected CRS
neighbor_bgs_centroids <- neighbor_bgs_proj %>%
  mutate(centroid = st_centroid(geometry)) %>%
  # Extract X and Y coordinates
  mutate(
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2]
  ) %>%
  # Arrange top → bottom (y descending), left → right (x ascending)
  arrange(desc(y), x) %>%
  mutate(index = row_number()) %>%
  select(-centroid, -x, -y)  # remove temporary columns if desired

# Plot with index labels
ggplot() +
  geom_sf(data = community_sf_proj, fill = "lightblue") +
  geom_sf(data = neighbor_bgs_centroids, color = "red", fill = NA) +
  geom_sf_text(data = neighbor_bgs_centroids, aes(label = index), size = 3, color = "black") +
  labs(title = paste("Block Groups within Community:", community_names_str))
