# Initialization
## set default options
options(stringsAsFactors = FALSE)

## set parameters
species_template_path <- "templates/species-template.txt"
species_path <- dir("data/species", "^.*\\.xlsx", full.names = TRUE)[1]
taxonomy_path <- dir("data/taxonomy","^.*\\.xlsx$", full.names = TRUE)[1]
study_area_path <- dir("data/study-area", "^.*\\.shp$", full.names = TRUE)[1]
unzip(dir("data/records", "^.*\\.zip$", full.names = TRUE), exdir = tempdir())
record_path <- dir(tempdir(), "^.*\\.csv$", full.names = TRUE)
elevation_path <- dir("data/elevation", "^.*\\.grd$", full.names = TRUE)[1]

## load packages
library(dplyr)
library(sf)

## source functions
source("code/functions/format_ebird_records.R")
source("code/functions/format_ebird_taxonomy.R")
source("code/functions/format_species_data.R")
source("code/functions/species_graph.R")
source("code/functions/species_map.R")
source("code/functions/species_widget.R")

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::st_read(study_area_path),
                                    parameters$crs)
record_data <- data.table::fread(record_path, data.table = FALSE)
taxonomy_data <- readxl::read_excel(taxonomy_path, sheet = 1)
species_data <- readxl::read_excel(species_path, sheet = 1)
elevation_data <- raster::raster(elevation_path)

## format species data
species_data <- do.call(format_species_data,
                        append(list(x = species_data), parameters$species))

## format record data
record_data <- do.call(format_ebird_records,
                       append(list(x = record_data,
                                   study_area = study_area_data),
                              parameters$records))

## format taxonomy data
taxonomy_data <- do.call(format_ebird_taxonomy,
                         append(list(x = taxonomy_data),
                                parameters$taxonomy))

## subset data if required
if (!identical(parameters$number_species, "all"))
  species_data <- species_data[seq_len(parameters$number_species), ,
                               drop = FALSE]

## discard unused species
record_data <- record_data[record_data$species_scientific_name %in%
                           species_data$species_scientific_name, ,
                           drop = FALSE]
taxonomy_data <- taxonomy_data[taxonomy_data$species_scientific_name %in%
                               species_data$species_scientific_name, ,
                               drop = FALSE]

## order taxonomy data by species data
taxonomy_data <- taxonomy_data[match(taxonomy_data$species_scientific_name,
                                     species_data$species_scientific_name), ,
                               drop = FALSE]

## verify that all species are accounted
assertthat::assert_that(
  setequal(taxonomy_data$species_scientific_name,
           species_data$species_scientific_name),
  msg = paste0("missing taxonomy data for: ",
               paste(setdiff(taxonomy_data$species_scientific_name,
                       species_data$species_scientific_name), collapse = ", ")))

## create spatial data representing landmasses around Brisbane
land_data <- rnaturalearth::ne_countries(country = "australia", scale = 10,
                                         returnclass = "sf")
land_data <- sf::st_transform(land_data, sf::st_crs(study_area_data))
land_data <- sf::st_intersection(land_data,
  sf::st_as_sfc(sf::st_bbox(sf::st_buffer(study_area_data, 200000))))

## create grid overlay for plotting distribution of records
grid_extent <- c(sf::st_bbox(study_area_data))
grid_extent <- c(grid_extent[1], grid_extent[2],
                 grid_extent[1] + round(abs(grid_extent[3] - grid_extent[1]) /
                                        parameters$grid_resolution) *
                                        parameters$grid_resolution,
                 grid_extent[2] + round(abs(grid_extent[4] - grid_extent[2]) /
                                        parameters$grid_resolution) *
                                        parameters$grid_resolution)
grid_data <- raster::raster(xmn = grid_extent[1], xmx = grid_extent[3],
                            ymn = grid_extent[2], ymx = grid_extent[4],
                            crs = as(study_area_data, "Spatial")@proj4string,
                            res = rep(parameters$grid_resolution, 2))
grid_data <- raster::setValues(grid_data, NA_real_)

## extract elevation data
elevation_data <- raster::crop(elevation_data,
  raster::extent(as(sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_buffer(
    study_area_data, 200000), 4326))), "Spatial")))
record_pts <- as(record_data[, c("year")], "Spatial")
elevation_data <- raster::projectRaster(elevation_data, method = "bilinear",
                                        crs = record_pts@proj4string)
elevation_data[raster::Which(elevation_data < 0)] <- 0
record_data$elevation <- raster::extract(elevation_data, record_pts)
rm(record_pts, elevation_data)

## create file names to save images/widgets
file_names <- species_data$species_scientific_name
file_names <- gsub("(", "", file_names, fixed = TRUE)
file_names <- gsub(")", "", file_names, fixed = TRUE)
file_names <- gsub("/", "", file_names, fixed = TRUE)
file_names <- gsub(" ", "-", file_names, fixed = TRUE)
file_names <- gsub(".", "", file_names, fixed = TRUE)
file_names <- paste0(taxonomy_data$order_scientific_name, "-",
                     taxonomy_data$family_scientific_name, "-", file_names)

# Exports
## create interactive maps for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_widget(species_data$species_scientific_name[i], record_data,
                      grid_data, study_area_data)
  saveRDS(p, paste0("assets/widgets/", file_names[i], ".rds"),
          compress = "xz")
  TRUE
})

## create static maps for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_map(species_data$species_scientific_name[i], record_data,
                   grid_data, land_data, study_area_data)
  ggplot2::ggsave(paste0("assets/maps/", file_names[i], ".png"), p,
                  width = parameters$map_width, height = parameters$map_width,
                  units = "in")
  TRUE
})

## create graphs for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_graph(species_data$species_scientific_name[i], record_data)
  ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".png"), p,
                  width = parameters$graph_width,
                  height = parameters$graph_height, units = "in")
  TRUE
})
