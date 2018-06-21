# Initialization
## set default options
options(stringsAsFactors = FALSE)

## create temporary directories
tmp1 <- file.path(tempdir(), basename(tempfile(fileext = "")))
tmp2 <- file.path(tempdir(), tempfile(fileext = ""))
tmp3 <- file.path(tempdir(), tempfile(fileext = ""))
tmp4 <- file.path(tempdir(), tempfile(fileext = ""))
dir.create(tmp1, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp2, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp3, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp4, showWarnings = FALSE, recursive = TRUE)

## set parameters
species_template_path <- "templates/species-template.txt"
species_path <- dir("data/species", "^.*\\.xlsx", full.names = TRUE)[1]
unzip(dir("data/study-area", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp1)
study_area_path <- dir(tmp1, "^.*\\.shp$", full.names = TRUE)[1]
unzip(dir("data/vegetation", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp2)
vegetation_path <- dir(tmp2, "^.*\\.shp$", full.names = TRUE)[1]
vegetation_class_path <- dir("data/vegetation", "^.*\\.xlsx$",
                             full.names = TRUE)[1]
unzip(dir("data/records", "^.*\\.zip$", full.names = TRUE), exdir = tmp3)
record_path <- dir(tmp3, "^.*\\.txt$", full.names = TRUE)
unzip(dir("data/land", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp4)
land_path <- dir(tmp4, "^.*\\.shp$", full.names = TRUE)[1]
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
source("code/functions/color_numeric_palette.R")
source("code/functions/ymax.R")
source("code/functions/breaks.R")
source("code/functions/addLegend_custom.R")

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::st_read(study_area_path),
                                    parameters$crs)
land_data <- sf::st_transform(sf::st_read(land_path), parameters$crs)
record_data <- data.table::fread(record_path, data.table = FALSE)
species_data <- readxl::read_excel(species_path, sheet = 1)
elevation_data <- raster::raster(elevation_path)
vegetation_data <- sf::st_transform(sf::st_read(vegetation_path),
                                    parameters$crs)

## format study area
study_area_data <- study_area_data %>%
                   sf::st_union() %>%
                   lwgeom::st_make_valid() %>%
                   lwgeom::st_snap_to_grid(1) %>%
                   sf::st_simplify(100) %>%
                   sf::st_collection_extract(type = "POLYGON") %>%
                   lwgeom::st_make_valid()
study_area_data <- sf::st_sf(name = "Brisbane", geometry = study_area_data)

## format land data
bbox_data <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(study_area_data, 200000)))
land_data <- land_data[as.matrix(sf::st_intersects(land_data,
                                                   bbox_data))[, 1], ]
land_data <- land_data %>%
             lwgeom::st_make_valid() %>%
             lwgeom::st_snap_to_grid(1) %>%
             sf::st_simplify(100) %>%
             sf::st_collection_extract(type = "POLYGON") %>%
             lwgeom::st_make_valid() %>%
             sf::st_intersection(bbox_data) %>%
             sf::st_collection_extract(type = "POLYGON") %>%
             lwgeom::st_make_valid() %>%
             sf::st_union()
land_data <- sf::st_sf(name = "Land", geometry = land_data)

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
grid_data[raster::extract(grid_data, as(study_area_data, "Spatial"),
                          cellnumbers = TRUE)[[1]][, 1]] <- 1

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
  levels = unique(vegetation_data[[parameters$vegetation$class_column_name]]))

## create file names to save images/widgets
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
  p <- species_widget(species_data$species_scientific_name[i], species_data,
                      record_data, grid_data, study_area_data,
                      parameters$maps$minimum_required_checklists,
                      parameters$maps$minimum_required_records)
  saveRDS(p, paste0("assets/widgets/", file_names[i], ".rds"),
          compress = "xz")
  TRUE
})

## create static maps for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_map(species_data$species_scientific_name[i], species_data,
                   record_data, grid_data, land_data, study_area_data,
                   parameters$maps$minimum_required_checklists,
                   parameters$maps$minimum_required_records)
  n <- as.character(stringr::str_count(species_data$maps[i], "_") + 1)
  ggplot2::ggsave(paste0("assets/maps/", file_names[i], ".png"), p,
                  width = parameters$maps$size[[n]]$width,
                  height = parameters$maps$size[[n]]$height,
                  units = "in")
  TRUE
})

## create graphs for each species
result <- vapply(seq_len(nrow(species_data)), FUN.VALUE = logical(1),
                 function(i) {
  p <- species_graph(species_data$species_scientific_name[i], species_data,
                     record_data)
  n <- as.character(stringr::str_count(species_data$graphs[i], "_") + 1)
  ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".png"), p,
                  width = parameters$graphs$size[[n]]$width,
                  height = parameters$graphs$size[[n]]$height,
                  units = "in")
  TRUE
})
