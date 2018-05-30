# Initialization
## set default options
options(stringsAsFactors = FALSE)

## set parameters
species_template_path <- "templates/species-template.txt"
species_path <- dir("data/species", "^.*\\.xlsx", full.names = TRUE)[1]

## set command line arguments
cmd_args <- commandArgs(trailingOnly = TRUE)
if (length(cmd_args) > 0) {
  overwrite <- as.logical(cmd_args[1])
} else {
  overwrite <- FALSE
}

## load packages
library(dplyr)
library(sf)

## source functions
source("code/functions/format_species_data.R")

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
species_data <- readxl::read_excel(species_path, sheet = 1)

## load templates
species_template_data <- readLines(species_template_path)
species_template_data <- paste(species_template_data, collapse = "\n")

## format species data
species_data <- do.call(format_species_data,
                        append(list(x = species_data), parameters$species))

## subset data if required
if (!identical(parameters$number_species, "all"))
  species_data <- species_data[seq_len(parameters$number_species), ,
                               drop = FALSE]

## create files
rmd_paths <- species_data$species_scientific_name
rmd_paths <- gsub("(", "", rmd_paths, fixed = TRUE)
rmd_paths <- gsub(")", "", rmd_paths, fixed = TRUE)
rmd_paths <- gsub("/", "", rmd_paths, fixed = TRUE)
rmd_paths <- gsub(" ", "-", rmd_paths, fixed = TRUE)
rmd_paths <- gsub(".", "", rmd_paths, fixed = TRUE)
rmd_paths <- paste0(rmd_paths, ".Rmd")

# Main processing
## generate rmarkdown files using the template
if (overwrite) {
  file_index <- seq_along(rmd_paths)
} else {
  file_index <- which(!file.exists(rmd_paths))
}
rmd_texts <- vapply(species_data$species_scientific_name,
                    FUN.VALUE = character(1), gsub, pattern = "$$SPECIESNAME$$",
                    fixed = TRUE, x = species_template_data)

## create bookdown metadata file
bookdown_list <- list()
bookdown_list$book_filename <- "brisbane-bird-atlas"
bookdown_list$chapter_name <- ""
bookdown_list$rmd_files <- c("index.Rmd",
                             "preface.Rmd",
                             "introduction.Rmd",
                             "brisbanes-environment.Rmd",
                             "methods.Rmd",
                             "species-accounts.Rmd",
                             rmd_paths,
                             "gazetteer.Rmd",
                             "contributing.Rmd",
                             "references.Rmd")

# Exports
## save rmarkdown files
for (i in file_index)
  writeLines(rmd_texts[i], rmd_paths[i])

## save bookdown metadata yaml file
writeLines(yaml::as.yaml(bookdown_list), "_bookdown.yml")
