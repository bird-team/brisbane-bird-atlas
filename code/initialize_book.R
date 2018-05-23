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

## set coommand line arguments
cmd_args <- commandArgs(trailingOnly = TRUE)
if (length(cmd_args) > 0) {
  overwrite <- as.logical(cmd_args[1])
} else {
  overwrite <- FALSE
}

## load packages
library(yaml)
library(plyr)
library(dplyr)
library(sf)

## source functions
source("code/functions.R")

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::st_read(study_area_path), 4326)
record_data <- data.table::fread(record_path, data.table = FALSE)
taxonomy_data <- readxl::read_excel(taxonomy_path, sheet = 1)

## load templates
chapter_template_data <- readLines(chapter_template_path)
species_template_data <- readLines(species_template_path)

## format record data
record_data <- do.call(format_ebird_records,
                       append(list(x = record_data,
                                   study_area = study_area_data),
                              parameters$records))

## format taxonomy data
taxonomy_data <- do.call(format_ebird_taxonomy,
                         append(list(x = taxonomy_data),
                                parameters$taxonomy))

## define species names
species_names <- intersect(record_data$species_scientific_name,
                           taxonomy_data$species_scientific_name)

## subset taxonomy data if required
if (!identical(parameters$number_species, "all"))
  species_names <- species_names[seq_len(parameters$number_species)]

## align data sets
taxonomy_data <- taxonomy_data[taxonomy_data$species_scientific_name %in%
                               species_names, , drop = FALSE]
record_data <- record_data[record_data$species_scientific_name %in%
                           species_names, , drop = FALSE]

# Main processing
## generate rmarkdown files using the templates
rmd_paths <- lapply(unique(taxonomy_data$family_scientific_name), function(f) {
  ### subset data to family
  curr_data <- taxonomy_data[taxonomy_data$family_scientific_name == f, ,
                             drop = FALSE]
  ### extract order and species data
  order_name <- curr_data$order_scientific_name[1]
  species_names <- unique(curr_data$species_scientific_name)
  ### create file names
  file_names <- character(nrow(curr_data) + 1)
  file_names[1] <- paste0(order_name, "-", f)
  file_names[-1] <- paste0(order_name, "-", f, "-", species_names)
  file_names <- gsub("(", "", file_names, fixed = TRUE)
  file_names <- gsub(")", "", file_names, fixed = TRUE)
  file_names <- gsub("/", "", file_names, fixed = TRUE)
  file_names <- gsub(" ", "-", file_names, fixed = TRUE)
  file_names <- gsub(".", "", file_names, fixed = TRUE)
  file_names <- paste0(file_names, ".Rmd")
  ### create family chapter header
  writeLines(gsub("$$FAMILYNAME$$", f, chapter_template_data, fixed = TRUE),
             file.path(file_names[1]))
  ### create rmarkdown file for the species in the family
  if (overwrite) {
    pos <- seq_along(species_names)
  } else {
    pos <- which(!file.exists(file_names[-1]))
  }
  vapply(pos, FUN.VALUE = logical(1), function(i) {
    writeLines(gsub("$$SPECIESNAME$$", species_names[i],
               species_template_data, fixed = TRUE), file_names[i + 1])
    TRUE
  })
  ### return file names
  file_names
})

## unlist file names
rmd_paths <- unlist(rmd_paths, use.names = FALSE, recursive = TRUE)

## create list with bookdown metadata
bookdown_list <- list()
bookdown_list$book_filename <- "brisbane-bird-atlas"
bookdown_list$chapter_name <- "Chapter "
bookdown_list$rmd_files <- c("index.Rmd", rmd_paths)

# Exports
## save bookdown metadata yaml file
writeLines(yaml::as.yaml(bookdown_list), "_bookdown.yml")
