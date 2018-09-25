# Initialization
## set default options
options(stringsAsFactors = FALSE)

## load packages
library(dplyr)
library(sf)

## create temporary directories
tmp1 <- file.path(tempdir(), basename(tempfile(fileext = "")))
dir.create(tmp1, showWarnings = FALSE, recursive = TRUE)
unzip(dir("data/study-area", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp1)

## set parameters
study_area_path <- dir(tmp1, "^.*\\.shp$", full.names = TRUE)[1]

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load study area data
study_area_data <- sf::st_transform(sf::read_sf(study_area_path),
                                    parameters$crs)

## format study area
study_area_data <- study_area_data %>%
                   lwgeom::st_make_valid() %>%
                   lwgeom::st_snap_to_grid(1) %>%
                   sf::st_simplify(100) %>%
                   {suppressWarnings(
                     sf::st_collection_extract(., type = "POLYGON"))} %>%
                   lwgeom::st_make_valid()

# Main processing
## define grid extent
grid_extent <- c(sf::st_bbox(study_area_data))
grid_extent <- c(grid_extent[1], grid_extent[2],
                 grid_extent[1] + round(abs(grid_extent[3] - grid_extent[1]) /
                                        parameters$grid_resolution) *
                                        parameters$grid_resolution,
                 grid_extent[2] + round(abs(grid_extent[4] - grid_extent[2]) /
                                        parameters$grid_resolution) *
                                        parameters$grid_resolution)

## initialize grid
grid_data <- raster::raster(xmn = grid_extent[1], xmx = grid_extent[3],
                            ymn = grid_extent[2], ymx = grid_extent[4],
                            crs = as(study_area_data, "Spatial")@proj4string,
                            res = rep(parameters$grid_resolution, 2)) %>%
             raster::setValues(1) %>%
             as("SpatialPolygonsDataFrame") %>%
             as("sf") %>%
             sf::st_set_crs(sf::st_crs(study_area_data))

## assign marine grid cells
grid_data$marine <- grid_data %>%
                    sf::st_intersects(study_area_data %>%
                                      filter(name == "marine")) %>%
                    as.matrix() %>%
                    c()

## assign terrestrial grid cells
grid_data$land <- grid_data %>%
                  sf::st_intersects(study_area_data %>%
                                    filter(name == "land")) %>%
                  as.matrix() %>%
                  c()

## remove cells where neither marine or land
grid_data <- grid_data %>% filter(land | marine)

## assign unique integer identifier
grid_data$id <- seq_len(nrow(grid_data))

## assign name field
grid_data$name <- paste ("Grid cell:", grid_data$id)

## format columns
grid_data <- grid_data %>% select(id, type, name)

# Exports
sf::write_sf(grid_data, "data/grid-data/grid.shp", delete_layer = TRUE)
