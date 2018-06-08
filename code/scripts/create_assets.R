# Initialization
## set default options
options(stringsAsFactors = FALSE)

## set parameters
species_template_path <- "templates/species-template.txt"
species_path <- dir("data/species", "^.*\\.xlsx", full.names = TRUE)[1]
study_area_path <- dir("data/study-area", "^.*\\.shp$", full.names = TRUE)[1]
unzip(dir("data/vegetation", "^.*\\.zip$", full.names = TRUE),
          exdir = tempdir())
vegetation_path <- dir(tempdir(), "^.*\\.shp$", full.names = TRUE)[1]
vegetation_class_path <- dir("data/vegetation", "^.*\\.xlsx$",
                             full.names = TRUE)[1]
unzip(dir("data/records", "^.*\\.zip$", full.names = TRUE), exdir = tempdir())
record_path <- dir(tempdir(), "^.*\\.csv$", full.names = TRUE)
elevation_path <- dir("data/elevation", "^.*\\.grd$", full.names = TRUE)[1]

## load packages
library(dplyr)
library(sf)
library(patchwork)

## source functions
source("code/functions/format_ebird_records.R")
source("code/functions/format_species_data.R")
source("code/functions/species_graph.R")
source("code/functions/species_map.R")
source("code/functions/species_widget.R")
source("code/functions/species_table.R")

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::st_read(study_area_path),
                                    parameters$crs)
record_data <- data.table::fread(record_path, data.table = FALSE)
species_data <- readxl::read_excel(species_path, sheet = 1)
elevation_data <- raster::raster(elevation_path)
vegetation_data <- sf::st_transform(sf::st_read(vegetation_path),
                                    parameters$crs)

## format species data
species_data <- do.call(format_species_data,
                        append(list(x = species_data), parameters$species))

## format record data
record_data <- do.call(format_ebird_records,
                       append(list(x = record_data,
                                   study_area = study_area_data),
                              parameters$records))

## subset data if required
if (!identical(parameters$number_species, "all"))
  species_data <- species_data[seq_len(parameters$number_species), ,
                               drop = FALSE]

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

## extract spatial attributes
locality_data <- record_data[!duplicated(record_data$locality), "locality"]

### extract elevation data
elevation_data <- raster::crop(elevation_data,
  raster::extent(as(sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_buffer(
    study_area_data, 200000), 4326))), "Spatial")))
locality_pts <- as(locality_data, "Spatial")
elevation_data <- raster::projectRaster(elevation_data, method = "bilinear",
                                        crs = locality_pts@proj4string)
elevation_data[raster::Which(elevation_data < 0)] <- 0
elevation_data[raster::Which(is.na(elevation_data))] <- 0
locality_data$elevation <- raster::extract(elevation_data, locality_data)
rm(locality_pts, elevation_data)

### extract vegetation data
locality_pos <- max.col(cbind(as.matrix(sf::st_intersects(locality_data,
                                                          vegetation_data)),
                              FALSE), ties.method = "last")
locality_data$vegetation_class <- vegetation_data[[
  parameters$vegetation$class_column_name]][locality_pos]
assertthat::assert_that(assertthat::noNA(locality_data$vegetation_class),
  msg = "all eBird records must overlap with vegetation data")

### merge event data with record data
locality_data <- as.data.frame(locality_data)
locality_data <- locality_data[, names(locality_data) != "geometry"]
record_data <- left_join(record_data, locality_data, by = "locality")
record_data$vegetation_class <- factor(record_data$vegetation_class,
  levels = parameters$vegetation$classes)

## create file names to save images/widgets
print("here 4")
file_names <- species_data$species_scientific_name
file_names <- gsub("(", "", file_names, fixed = TRUE)
file_names <- gsub(")", "", file_names, fixed = TRUE)
file_names <- gsub("/", "", file_names, fixed = TRUE)
file_names <- gsub(" ", "-", file_names, fixed = TRUE)
file_names <- gsub(".", "", file_names, fixed = TRUE)

# Exports
## create tables
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_table(species_data$species_scientific_name[i], species_data,
                     record_data, grid_data)
  saveRDS(p, paste0("assets/tables/", file_names[i], ".rds"),
          compress = "xz")
  TRUE
})

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
                  width = parameters$map_size$width,
                  height = parameters$map_size$height,
                  units = "in")
  TRUE
})

## create graphs for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_graph(species_data$species_scientific_name[i], species_data,
                     record_data)
  n <- stringr::str_count(species_data$graphs[i], "-") + 1
  ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".png"), p,
                  width = parameters$graph_size[[n]]$width,
                  height = parameters$graph_size[[n]]$height,
                  units = "in")
  TRUE
})
