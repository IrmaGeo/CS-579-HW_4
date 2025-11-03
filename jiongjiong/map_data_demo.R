# Comment out below if they are not installed:
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("curl", type = "source")
# install.packages("tigris")
# install.packages("patchwork")

library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)
library(tidycensus)
library(tidyverse)
library(tigris)
library(patchwork)
library(yaml)
library(tibble)

options(tigris_use_cache = TRUE)

getwd()

config_file_path <- file.path(getwd(), 'config.yaml')
stopifnot(file.exists(config_file_path))
config <- yaml::read_yaml(config_file_path)


census_api_key_file_path <- file.path(getwd(), 'census_api_key.txt')

stopifnot(file.exists(census_api_key_file_path))

census_api_key <- trimws(read_file(census_api_key_file_path))
census_api_key(census_api_key, install = FALSE)

# Data is downloaded from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
# Download steps:
#     1. Click "Export" button
#     2. Export as shapefile.
#     3. Unzip the download zip file.

boundary_file_path <- file.path(getwd(), 'Boundaries - Community Areas_20251102/geo_export_78bb31e8-e65e-47ef-bb49-90dbc4911759.shp')

stopifnot(file.exists(boundary_file_path))

chi_ca_sf <- sf::st_read(boundary_file_path)
# Converts all coordinates into North America NAD83 CRS
chi_ca_proj_sf <- sf::st_transform(chi_ca_sf, crs=4269)


# chi_ca_file1 <- file.path(getwd(), 'Boundaries_CommunityAreas_20251004/geo_export_2f67acac-76d9-47a1-9c12-fcf8534da850.shp')

# file.exists(chi_ca_file1)

# chi_ca_sf1 <- sf::st_read(chi_ca_file1)
# nrow(chi_ca_sf1)
# head(chi_ca_sf1)

st_crs(chi_ca_sf)

# suggest_crs(chi_ca_sf)

# chi_ca_proj_sf <- sf::st_transform(chi_ca_sf, crs=4269)
# nrow(chi_ca_proj_sf)


# head(chi_ca_proj_sf)

ggplot(chi_ca_proj_sf) +
    geom_sf() +
    labs(title = "Chicago Community Areas NAD83")

# community_names <- c("WEST GARFIELD PARK", "EAST GARFIELD PARK")
# community_names <- c('South Lawndale', 'Hermosa', 'EAST GARFIELD PARK')
community_names <- c('EAST GARFIELD PARK')
community_names_str <- paste(community_names, collapse = ", ")

community_sf <- dplyr::filter(chi_ca_proj_sf, community %in% community_names)
nrow(community_sf)

head(community_sf)

ggplot(community_sf) +
    geom_sf() +
    labs(title = paste("Community Areas:", community_names_str))



# 1 H1_001N " !!Total:" OCCUPANCY STATUS
# P1_001N !!Total: RACE
vars_pl_2020 = load_variables(2020, "pl", cache=TRUE)
# PCT023001 Total RACE
# H003001 Total OCCUPANCY STATUS
# P008001 Total RACE
vars_sf1_2010 = load_variables(2010, "sf1", cache=TRUE)
vars_sf2_2010 = load_variables(2010, "sf2", cache=TRUE)

# write.csv(vars_pl_2020, file.path(getwd(),"vars_pl_2020.csv"))

# write.csv(vars_sf1_2010, file.path(getwd(),"vars_sf1_2010.csv"))

# write.csv(vars_sf2_2010, file.path(getwd(),"vars_sf2_2010.csv"))

common_decennial_vars = inner_join(vars_pl_2020, vars_sf1_2010, by="concept", relationship = "many-to-many")


# Grow block groups until reaching at least min_n
grow_block_groups <- function(curr_block_groups,
                              all_block_groups,
                              min_n = 60) {
  curr_bg <- curr_block_groups
  all_bg <- all_block_groups

  while (nrow(curr_bg) < min_n) {
    # print(nrow(curr_bg))
    # Replace curr_bg with all blocks that intersect the current selection
    curr_bg <- all_bg[lengths(st_intersects(all_bg, curr_bg)) > 0, ]

    # Stop if we reached min_n
    if (nrow(curr_bg) >= min_n) break
  }

  return(curr_bg)
}

get_decennial_data <- function(config, community_sf) {
    decennial_config <- config$history$decennial
    geography_unit <- decennial_config$geography_unit
    categories <- decennial_config$categories

    results_list <- list()

    for (category_idx in seq_along(categories)) {
        category_info <- decennial_config$categories[[category_idx]]
        variables <- category_info$variables

        category_results <- list()

        for (variable_idx in seq_along(variables)) {
            variable <- variables[[variable_idx]]
            variable_name <- variable$variable_name
            year <- variable$year
            raw_data <- get_decennial(
                geography   = geography_unit,
                variables   = variable_name,
                year        = year,
                state       = "IL",
                county      = "Cook",
                # Include shapefile for mapping
                geometry    = TRUE,
                cache_table = TRUE
            )

            is_intersect <- st_intersects(raw_data, community_sf)
            data <- raw_data[lengths(is_intersect) > 0, ]

            # Store result for this variable
            category_results[[length(category_results) + 1]] <- list(
                variable_idx   = variable_idx,
                variable_name  = variable_name,
                year           = year,
                data           = data
            )
        }

        results_list[[length(results_list) + 1]] <- list(
            category_idx     = category_idx,
            category_name    = category_info$category_name,
            description      = category_info$description,
            category_results = category_results
        )
    }

    return(results_list)
}


get_acs_data <- function(config, community_sf) {
    acs_config <- config$history$acs
    geography_unit <- acs_config$geography_unit
    survey <- acs_config$survey
    categories <- acs_config$categories

    results_list <- list()

    for (category_idx in seq_along(categories)) {
        category_info <- acs_config$categories[[category_idx]]
        variables <- category_info$variables

        category_results <- list()

        for (variable_idx in seq_along(variables)) {
            variable <- variables[[variable_idx]]
            variable_name <- variable$variable_name
            year <- variable$year

            # print(paste("Variable:", variable_name,
            #             "Year:", year))

            raw_data <- get_acs(
                geography = geography_unit,
                variables = variable_name,
                year      = year,
                state     = "IL",
                county    = "Cook",
                # Include shapefile for mapping
                geometry  = TRUE,
                survey = survey
            )

            is_intersect <- st_intersects(raw_data, community_sf)
            filtered_data <- raw_data[lengths(is_intersect) > 0, ]

            data = grow_block_groups(filtered_data, raw_data)

            # Store result for this variable
            category_results[[length(category_results) + 1]] <- list(
                variable_idx   = variable_idx,
                variable_name  = variable_name,
                year           = year,
                data           = data
            )
        }

        results_list[[length(results_list) + 1]] <- list(
            category_idx     = category_idx,
            category_name    = category_info$category_name,
            description      = category_info$description,
            category_results = category_results
        )
    }

    return(results_list)
}


generate_images <- function(history, data_type) {
    for (category_results_info in history) {
        category_name <- category_results_info$category_name
        description <- category_results_info$description
        data_info_list <- category_results_info$category_results

        years <- c()             # Initialize empty vector
        plot_images <- list()    # Store plots

        # Determine overall min and max fill_value for this category
        fill_min <- Inf
        fill_max <- -Inf

        for (data_info_idx in seq_along(data_info_list)) {
            data_info <- data_info_list[[data_info_idx]]

            year <- data_info$year
            data <- data_info$data
            # print(paste("Variable:", data_info$variable_name,
            #             "Year:", year))
            title <- paste(description, 'in', year)

            # Only last plot shows legend
            show_legend <- ifelse(data_info_idx == length(data_info_list), TRUE, FALSE)

            data$fill_value <- if (data_type == "decennial") data$value else data$estimate

            fill_min <- min(fill_min, min(data$fill_value, na.rm = TRUE))
            fill_max <- max(fill_max, max(data$fill_value, na.rm = TRUE))

            plot_image <- ggplot() +
                geom_sf(data = data, aes(fill = fill_value), color = NA) +
                geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
                # scale_fill_viridis_c(name = category_name, option = "plasma") +
                # scale_fill_gradient(name = category_name,
                #                     low = "green", high = "yellow") +
                scale_fill_distiller(name=category_name,
                                     palette = "YlGn",
                                     direction = -1,
                                     limits = c(fill_min, fill_max),
                                     guide = if (show_legend) "colourbar" else "none") +
                ggtitle(title) +
                theme_minimal() +
                theme(
                axis.text = element_blank(),
                axis.ticks = element_blank()
                )

            # Add plot to list
            plot_images[[length(plot_images) + 1]] <- plot_image

            # Add year to years vector
            years <- c(years, data_info$year)
        }

        # Horizontal combine
        combined_plot <- wrap_plots(plot_images,
                                    ncol = length(plot_images),
                                    guides = "collect") +
            plot_annotation(
                title = paste(str_to_title(community_names_str),
                    category_name,
                    "in",
                    paste(years, collapse = " vs ")),
                theme = theme(
                    plot.title = element_text(
                        hjust = 0.5,       # center horizontally
                        face = "plain",    # remove bold
                        size = 14
                    )
                )
            ) +
            theme(
                panel.background = element_rect(fill = "white"),    # remove gray/black panel background
                plot.background  = element_rect(fill = "white", color = NA)  # remove surrounding background
            )

        # Save as PNG
        file_stem <- paste(data_type,
                           gsub(" ", "_", tolower(category_name)),
                           paste(years, collapse = "_"),
                           sep="_")
        file_name <- paste0(file_stem, ".png")

        ggsave(file.path(getwd(), file_name),
               combined_plot,
               width = 10, height = 3, dpi = 300)
    }
}


decennial_history <- get_decennial_data(config, community_sf)
generate_images(decennial_history, 'decennial')

acs_history <- get_acs_data(config, community_sf)
generate_images(acs_history, 'acs')


for (category_results_info in acs_history) {
    category_name <- category_results_info$category_name
    description <- category_results_info$description
    data_info_list <- category_results_info$category_results

    for (data_info in data_info_list) {
        year <- data_info$year
        data <- data_info$data

        year



acs_history[[1]][["category_results"]][[2]][["data"]]


block_race_populations_2010 <- get_decennial(
  geography   = "block",
  variables   = "P008001",  # Total population (PL94-171)
  year        = 2010,
  state       = "IL",
  county      = "Cook",
  geometry    = TRUE,         # Include shapefile for mapping
  cache_table = TRUE
)

block_race_populations_2020 <- get_decennial(
  geography   = "block",
  variables   = "P1_001N",  # Total population (PL94-171)
  year        = 2020,
  state       = "IL",
  county      = "Cook",
  geometry    = TRUE,         # Include shapefile for mapping
  cache_table = TRUE
)


# intersections = st_intersects(block_race_populations_2020, community_sf)

# community_block_race_populations_2020 <- block_race_populations_2020[lengths(intersections) > 0, ]

# Find 2020 blocks within community
block_within_community_2020 <- st_intersects(block_race_populations_2020, community_sf)

# Filter only those blocks
community_block_race_populations_2020 <- block_race_populations_2020[lengths(block_within_community_2020) > 0, ]

# Find 2010 blocks within community
block_within_community_2010 <- st_intersects(block_race_populations_2010, community_sf)

# Filter only those blocks
community_block_race_populations_2010 <- block_race_populations_2010[lengths(block_within_community_2010) > 0, ]

# Plot
p2010 <- ggplot() +
  geom_sf(data = community_block_race_populations_2010, aes(fill = value), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  # scale_fill_viridis_c(name = "Population in 2010", option = "plasma") +
  # scale_fill_gradient(name = "Population in 2010",
  #                     low = "green", high = "yellow") +
    scale_fill_distiller(name = "Population in 2010", palette = "YlGn", direction = -1) +

  ggtitle("Total Population of all Races (2010)") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p2020 <- ggplot() +
  geom_sf(data = community_block_race_populations_2020, aes(fill = value), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  # scale_fill_viridis_c(name = "Population in 2020", option = "plasma") +
  # scale_fill_gradient(name = "Population in 2020",
  #                     low = "green", high = "yellow") +
  scale_fill_distiller(name = "Population in 2020", palette = "YlGn", direction = -1) +
  ggtitle("Total Population of all Races (2020)") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Combine plots
population_plot <- p2010 | p2020

# Save as PNG
ggsave(file.path(getwd(), "population_2010_2020.png"), population_plot,
       width = 10, height = 3, dpi = 300)

ggplot() +
    geom_freqpoly(data = community_block_race_populations_2010,
                aes(x = value, color = "2010"),
                binwidth = binwidth, linewidth = 1.2) +
    geom_freqpoly(data = community_block_race_populations_2020,
                aes(x = value, color = "2020"),
                binwidth = binwidth, linewidth = 1.2) +
    scale_color_manual(name = "Year", values = c("2010" = "green", "2020" = "yellow")) +
    ggtitle("Population of all Races Comparison: 2010 vs 2020") +
    xlab("Population of all Races per Block") +
    ylab("Number of Blocks") +
    theme_minimal()



#
block_houses_2010 <- get_decennial(
  geography = "block",
  variables = "H003001",  # Total housing units (PL94-171)
  year      = 2010,
  state     = "IL",
  county    = "Cook",
  geometry  = TRUE         # Include shapefile for mapping
)

block_houses_2020 <- get_decennial(
  geography = "block",
  variables = "H1_001N",  # Total housing units (PL94-171)
  year      = 2020,
  state     = "IL",
  county    = "Cook",
  geometry  = TRUE         # Include shapefile for mapping
)

# Find 2010 blocks within community
block_within_community_2010 <- st_within(block_houses_2010, community_sf)

# Filter only those blocks
community_block_houses_2010 <- block_houses_2010[lengths(block_within_community_2010) > 0, ]

# Find 2020 blocks within community
houses_block_within_community_2020 <- st_within(block_houses_2020, community_sf)

# Filter only those blocks
community_block_houses_2020 <- block_houses_2020[lengths(houses_block_within_community_2020) > 0, ]

# Plot
plot_houses_2010 <- ggplot() +
  geom_sf(data = community_block_houses_2010, aes(fill = value), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  scale_fill_distiller(name = "Housing Units in 2010", palette = "YlGn", direction = -1) +
  ggtitle("Total Number of Housing Units (2010)") +
  theme_minimal()

plot_houses_2020 <- ggplot() +
  geom_sf(data = community_block_houses_2020, aes(fill = value), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  scale_fill_distiller(name = "Housing Units in 2020", palette = "YlGn", direction = -1) +
  ggtitle("Total Number of Housing Units (2020)") +
  theme_minimal()

# Combine plots
house_plot <- plot_houses_2010 | plot_houses_2020

# Save as PNG
ggsave(file.path(getwd(), "houses_2010_2020.png"), house_plot,
       width = 12, height = 6, dpi = 300)

ggplot() +
    geom_freqpoly(data = community_block_houses_2010,
                aes(x = value, color = "2010"),
                binwidth = binwidth, linewidth = 1.2) +
    geom_freqpoly(data = community_block_houses_2020,
                aes(x = value, color = "2020"),
                binwidth = binwidth, linewidth = 1.2) +
    scale_color_manual(name = "Year", values = c("2010" = "green", "2020" = "yellow")) +
    ggtitle("Housing Units Counts Comparison: 2010 vs 2020") +
    xlab("Housing Units per Block") +
    ylab("Number of Blocks") +
    theme_minimal()



vars_acs5_2010 <- load_variables(2010, "acs5")
vars_acs5_block_group_2010 <- vars_acs5_2010 %>%
  filter(geography == "block group")

vars_acs5_2015 <- load_variables(2015, "acs5", cache=TRUE)
vars_acs5_block_group_2015 <- vars_acs5_2015 %>%
  filter(geography == "block group")

vars_acs5_2020 <- load_variables(2020, "acs5")
vars_acs5_block_group_2020 <- vars_acs5_2020 %>%
  filter(geography == "block group")


vars_acs5_2023 <- load_variables(2023, "acs5", cache=TRUE)
vars_acs5_block_group_2023 <- vars_acs5_2023 %>%
    filter(geography == "block group")


vars_acs1_2023 <- load_variables(2023, "acs1", cache=TRUE)

# B01001_001 Estimate!!Total SEX BY AGE
# B01003_001 Estimate!!Total TOTAL POPULATION
# B02001_001 Estimate!!Total RACE
# B25001_001 Estimate!!Total HOUSING UNITS
# B25018_001 Estimate!!Median number of rooms
# B25058_001 Estimate!!Median contract rent
# B25064_001 Estimate!!Median gross rent
# B25077_001 Estimate!!Median value (dollars)

# B01001_002 Estimate!!Total!!Male SEX BY AGE
# B01001_026 Estimate!!Total!!Female SEX BY AGE
# B25001_001 Estimate!!Total HOUSING UNITS
# B01002_001 Estimate!!Median age!!Total MEDIAN AGE BY SEX
# B19127_001 Estimate!!Aggregate family income in the past 12 months
# B08303_001 Estimate!!Total TRAVEL TIME TO WORK
common_acs_vars = inner_join(vars_acs5_block_group_2015, vars_acs5_block_group_2023, by=c("name"))


# tract_populations_2010 <- get_acs(
#   geography = "tract",
#   variables = "B01003_001",
#   year      = 2010,
#   state     = "IL",
#   county    = "Cook",
#   geometry  = TRUE,         # Include shapefile for mapping
#   survey = "acs5"
# )

block_group_populations_2015 <- get_acs(
  geography = "block group",
  variables = "B01003_001",
  year      = 2015,
  state     = "IL",
  county    = "Cook",
  geometry  = TRUE,         # Include shapefile for mapping
  survey = "acs5"
)

block_group_populations_2023 <- get_acs(
  geography = "block group",
  variables = "B01003_001",
  year      = 2023,
  state     = "IL",
  county    = "Cook",
  geometry  = TRUE,         # Include shapefile for mapping
  survey = "acs5"
)

# Find 2015 block groups within community
block_within_community_2015 <- st_within(block_group_populations_2015, community_sf)

# Filter only those block groups
community_block_group_populations_2015 <- block_group_populations_2015[lengths(block_within_community_2015) > 0, ]

# Find 2023 block groups within community
block_within_community_2023 <- st_within(block_group_populations_2023, community_sf)

# Filter only those block groups
community_block_group_populations_2023 <- block_group_populations_2023[lengths(block_within_community_2023) > 0, ]




new_community_block_group_populations_2015 <- grow_block_groups(community_block_group_populations_2015, block_group_populations_2015)

new_community_block_group_populations_2023 <- grow_block_groups(community_block_group_populations_2023, block_group_populations_2023)


# Plot
plot_populations_2015 <- ggplot() +
  geom_sf(data = new_community_block_group_populations_2015, aes(fill = estimate), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  scale_fill_distiller(name = "Total Population in 2015", palette = "YlGn", direction = -1) +
  ggtitle("Total Population (2010)") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

plot_populations_2023 <- ggplot() +
  geom_sf(data = new_community_block_group_populations_2023, aes(fill = estimate), color = NA) +
  geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
  scale_fill_distiller(name = "Total Population in 2023", palette = "YlGn", direction = -1) +
  ggtitle("Total Population (2023)") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combine plots
populations_plot <- plot_populations_2015 | plot_populations_2023

# Save as PNG
ggsave(file.path(getwd(), "populations_2015_2023.png"), populations_plot,
       width = 12, height = 6, dpi = 300)

# ggplot() +
#     geom_freqpoly(data = new_community_block_group_populations_2015,
#                 aes(x = estimate, color = "2015"),
#                 binwidth = binwidth, linewidth = 1.2) +
#     geom_freqpoly(data = new_community_block_group_populations_2023,
#                 aes(x = estimate, color = "2023"),
#                 binwidth = binwidth, linewidth = 1.2) +
#     scale_color_manual(name = "Year", values = c("2015" = "green", "2023" = "yellow")) +
#     ggtitle("Total Population Comparison: 2015 vs 2023") +
#     xlab("Total Population per Block Group") +
#     ylab("Number of Block Groups") +
#     theme_minimal()

library(ggplot2)

# Set binwidth
binwidth <- 50  # adjust as needed

# Combine datasets with a "Year" column
combined <- rbind(
  data.frame(estimate = new_community_block_group_populations_2015$estimate, Year = "2015"),
  data.frame(estimate = new_community_block_group_populations_2023$estimate, Year = "2023")
)

# Overlapping histogram with named colors
ggplot(combined, aes(x = estimate, fill = Year)) +
  geom_histogram(binwidth = binwidth, color = "black", alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("2015" = "blue", "2023" = "green")) +
  ggtitle("Total Population Comparison: 2015 vs 2023") +
  xlab("Total Population per Block Group") +
  ylab("Number of Block Groups") +
  theme_minimal()

ks.test(x = new_community_block_group_populations_2015$estimate,
        y = new_community_block_group_populations_2023$estimate)


ks.test(x = new_community_block_group_populations_2015$moe,
        y = new_community_block_group_populations_2023$moe)

# Small D and large p-value

#
block_race_populations_2020 <- get_decennial(
  geography = "block",
  variables = "P1_001N",  # Total population (PL94-171)
  year      = 2020,
  state     = "IL",
  county    = "Cook",
  geometry  = TRUE         # Include shapefile for mapping
)



total_population_2010 <- get_decennial(
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
