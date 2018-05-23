# Initialization
## set default options
options(stringsAsFactors = FALSE)


## set parameters
species_template_path <- "templates/species-template.txt"
chapter_template_path <- "templates/chapter-template.txt"
taxonomy_path <- dir("data/taxonomy","^.*\\.xlsx$", full.names = TRUE)
study_area_path <- dir("data/study-area", "^.*\\.shp$", full.names = TRUE)
unzip(dir("data/records", "^.*\\.zip$", full.names = TRUE), exdir = tempdir())
record_path <- dir(tempdir(), "^.*\\.csv$", full.names = TRUE)

## load packages
library(dplyr)
library(sf)

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::st_read(study_area_path),
                                    parameters$crs)
record_data <- data.table::fread(record_path, data.table = FALSE)
taxonomy_data <- readxl::read_excel(taxonomy_path, sheet = 1)

## load templates
chapter_template_data <- readLines(chapter_template_path)
species_template_data <- readLines(species_template_path)

# Main processing
## format record data
record_data <- do.call(format_ebird_records,
                       append(list(x = record_data,
                                   study_area = study_area_data),
                              parameters$records))

## format taxonomy data
taxonomy_data <- do.call(format_ebird_taxonomy,
                         append(list(x = taxonomy_data),
                                parameters$taxonomy))

## subset taxonomy data if required
if (!identical(parameters$number_species, "all"))
  species_names <- species_names[seq_len(parameters$number_species)]

## create spatial data representing landmasses around Brisbane
land_data <- rnaturalearth::ne_countries(country = "australia", scale = 10,
                                         returnclass = "sf")
land_data <- sf::st_transform(land_data, sf::st_crs(study_area_data))
land_data <- sf::st_intersection(land_data,
  sf::st_as_sfc(sf::st_bbox(sf::st_buffer(study_area_data, 200000))))

## define species names
species_names <- intersect(record_data$species_scientific_name,
                           taxonomy_data$species_scientific_name)

# Exports
## create file names
file_names <- species_name
file_names <- gsub("(", "", file_names, fixed = TRUE)
file_names <- gsub(")", "", file_names, fixed = TRUE)
file_names <- gsub("/", "", file_names, fixed = TRUE)
file_names <- gsub(" ", "-", file_names, fixed = TRUE)
file_names <- gsub(".", "", file_names, fixed = TRUE)

## create interactive maps for each species
result <- vapply(species_names, FUN.VALUE = logical(1), function(i) {
  p <- plot_species_maps(species_name[i], record_data, grid_data, land_data,
                         TRUE)
  saveRDS(p, paste0("assets/interactive-maps/", file_names[i], ".rds"),
          compress = "xz")
  TRUE
})

## create static maps for each species
result <- vapply(seq_along(species_names), FUN.VALUE = logical(1), function(i) {
  p <- plot_species_maps(species_names[i], record_data, grid_data, land_data,
                         FALSE)
  ggplot2::ggsave(paste0("assets/static-maps/", file_names[i], ".png"), p,
                  width = 4.5, height = 4.5, units = "in")
  TRUE
})

## create graphs for each species
result <- vapply(seq_along(species_names), FUN.VALUE = logical(1), function(i) {
  p <- plot_graphs_maps(species_names[i], record_data, grid_data, land_data,
                         FALSE)
  ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".png"), p,
                  width = 2.5, height = 5.5, units = "in")
  TRUE
})
