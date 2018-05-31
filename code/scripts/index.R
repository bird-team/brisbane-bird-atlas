# Initialization
## set default options
options(stringsAsFactors = FALSE)

## set parameters
species_path <- dir("data/species", "^.*\\.xlsx", full.names = TRUE)[1]
audio_path <- dir("data/audio", "^.*\\.xlsx", full.names = TRUE)[1]

## source functions
source("code/functions/format_audio_data.R")
source("code/functions/format_species_data.R")
source("code/functions/species_title.R")
source("code/functions/render_species_graph.R")
source("code/functions/render_species_map.R")
source("code/functions/render_species_widget.R")
source("code/functions/render_species_audio.R")
source("code/functions/latex_commands.R")

## load packages
library(sf)
library(ggplot2)
library(leaflet)

# Preliminary processing
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
species_data <- readxl::read_excel(species_path, sheet = 1)
audio_data <- readxl::read_excel(audio_path, sheet = 1)

# Main processing
## format species data
species_data <- do.call(format_species_data,
                        append(list(x = species_data), parameters$species))

## format audio data
audio_data <- do.call(format_audio_data,
                      append(list(x = audio_data), parameters$audio))
