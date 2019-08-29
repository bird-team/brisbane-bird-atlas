# Initialization
## set default options
options(stringsAsFactors = FALSE)

### set slash symbol for printing
slash_symbol <- "/"
if (.Platform$OS.type == "windows")
  slash_symbol <- "\\"

## check that API settings configured for Google
if (identical(Sys.getenv("GOOGLE_TOKEN"), "")) {
  stop(paste0("'", Sys.getenv("HOME"), slash_symbol, ".Renviron' does not ",
              "contain the credentials for Google (i.e. GOOGLE_TOKEN ",
              "variable)"))
}

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
elevation_path <- dir("data/elevation", "^.*\\.tif$", full.names = TRUE)[1]
grid_path <- dir("data/grid", "^.*\\.shp$", full.names = TRUE)[1]
grid_metadata_path <- dir("data/grid", "^.*\\.xlsx$", full.names = TRUE)[1]
exclude_locality_path <- dir("data/localities", "^.*\\.xlsx$",
                             full.names = TRUE)[1]

## load packages
library(dplyr)
library(sf)
library(patchwork)
library(tictoc)

## setup run timer
tictoc::tic.clearlog()
tictoc::tic("Total run time")

## source functions
tictoc::tic("Source functions")
source("code/functions/format_grid_data.R")
source("code/functions/format_grid_metadata.R")
source("code/functions/format_ebird_records.R")
source("code/functions/format_species_data.R")
source("code/functions/add_reporting_rate_columns.R")
source("code/functions/add_detection_columns.R")
source("code/functions/species_graph.R")
source("code/functions/species_map.R")
source("code/functions/species_widget.R")
source("code/functions/species_table.R")
source("code/functions/grid_map.R")
source("code/functions/grid_summary_table.R")
source("code/functions/grid_checklist_table.R")
source("code/functions/find_observer_name.R")
source("code/functions/color_numeric_palette.R")
source("code/functions/ymax.R")
source("code/functions/breaks.R")
source("code/functions/addLegend_custom.R")
tictoc::toc(log = TRUE, quiet = TRUE)

# Preliminary processing
tictoc::tic("Preliminary processing")

## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

## load data
study_area_data <- sf::st_transform(sf::read_sf(study_area_path),
                                    parameters$crs)
grid_data <- sf::read_sf(grid_path)
grid_metadata <- readxl::read_excel(grid_metadata_path, sheet = 1)
exclude_locality_data <- readxl::read_excel(exclude_locality_path, sheet = 1)
land_data <- sf::st_transform(sf::read_sf(land_path), parameters$crs)
record_data <- data.table::fread(record_path, quote = "", data.table = FALSE)
species_data <- readxl::read_excel(species_path, sheet = 1)
elevation_data <- raster::raster(elevation_path)
vegetation_data <- sf::st_transform(sf::read_sf(vegetation_path),
                                    parameters$crs)

## format study area
study_area_data <- study_area_data %>%
                   sf::st_set_precision(1000) %>%
                   lwgeom::st_make_valid()

study_area_simple_data <- study_area_data %>%
                          sf::st_simplify(100) %>%
                          sf::st_set_precision(1000) %>%
                          lwgeom::st_make_valid() %>%
                          {suppressWarnings(sf::st_collection_extract(.,
                            type = "POLYGON"))}

## format land data
bbox_data <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(study_area_simple_data,
                                                     200000)))
land_data <- land_data[as.matrix(sf::st_intersects(land_data,
                                                   bbox_data))[, 1], ]
land_data <- land_data %>%
             lwgeom::st_make_valid() %>%
             lwgeom::st_snap_to_grid(1) %>%
             sf::st_simplify(100) %>%
             {suppressWarnings(sf::st_collection_extract(.,
                                                         type = "POLYGON"))} %>%
             lwgeom::st_make_valid() %>%
             {suppressWarnings(sf::st_intersection(., bbox_data))} %>%
             {suppressWarnings(sf::st_collection_extract(.,
                                                         type = "POLYGON"))} %>%
             lwgeom::st_make_valid() %>%
             sf::st_union()
land_data <- sf::st_sf(name = "Land", geometry = land_data)

## format grid data
grid_data <- do.call(format_grid_data,
                     append(list(x = grid_data,
                                 study_area_data = study_area_simple_data),
                            parameters["grid_resolution"]))

## format grid metadata
grid_metadata <- do.call(format_grid_metadata,
                     append(list(x = grid_metadata,
                                 grid_data = grid_data),
                            parameters[["grid_metadata"]]))

## add in metadata columns
grid_data <- dplyr::left_join(grid_data, grid_metadata, by = "id")

## format species data
species_data <- do.call(format_species_data,
                        append(list(x = species_data), parameters$species))

## format record data
record_data <- do.call(format_ebird_records,
                       append(list(x = record_data,
                                   study_area = grid_data %>%
                                                sf::st_union() %>%
                                                sf::st_sf(id = 1)),
                              parameters$records))

## exclude localities
exclude_column_name <-
  parameters$exclude_locality_list$locality_name_column_name
assertthat::assert_that(assertthat::has_name(exclude_locality_data,
                                             exclude_column_name))
record_data <- record_data %>%
               filter(!locality %in%
                      exclude_locality_data[[exclude_column_name]])

## format record data to extract locations
locations_data <-
  record_data %>%
  select(locality, locality_name, locality_type, longitude,
         latitude) %>%
  filter(!duplicated(locality)) %>%
  filter(!is.na(locality), !is.na(locality_name),
         !is.na(locality_type), !is.na(locality_name)) %>%
  filter(nchar(locality_name) > 0) %>%
  mutate(locality_name =
    vapply(locality_name, FUN.VALUE = character(1), function(x)
        paste(strwrap(x, parameters$surveyor_sheets$maps$label_character_width),
              collapse = "\n")))

## extract month year for latest checklist
latest_checklist_month_year <- max(as.POSIXct(strptime(record_data$date,
                                                       "%d/%m/%Y")))
latest_checklist_month_year <- format(latest_checklist_month_year, "%b %Y")

## verify that species have sufficient data
### verify that species have at least a minimum number of records
species_record_count <- sapply(seq_len(nrow(species_data)), function(i) {
  ### initialization
  records_starting_year <- species_data$records_starting_year[i]
  ### count number of records
  sum((record_data$species_scientific_name ==
       species_data$species_scientific_name[i]) &
      (record_data$year >= records_starting_year))
})
species_invalid_settings <- (species_record_count == 0) &
                            !is.na(species_data$maps) &
                            !is.na(species_data$graphs)
if (any(species_invalid_settings)) {
  stop(paste("The following species do not have a single record after the",
             "specified starting year but require maps and/or graphs:",
       paste(species_data$species_scientific_name[species_invalid_settings],
                   collapse = ", ")))
}

### verify that species have at least one record with abundance data
species_abundance_count <- sapply(seq_len(nrow(species_data)), function(i) {
  ### initialization
  records_starting_year <- species_data$records_starting_year[i]
  ### count number of records
  sum((record_data$species_scientific_name ==
       species_data$species_scientific_name[i]) &
      (record_data$year >= records_starting_year) &
       !is.na(record_data$count))
})
species_invalid_settings <- (species_abundance_count == 0) &
                             grepl("4", species_data$graphs)
if (any(species_invalid_settings)) {
  stop(paste("The following species do not have a single record with abundance",
             "data after the specified starting year but require graphs:",
       paste(species_data$species_scientific_name[species_invalid_settings],
                   collapse = ", ")))
}

### verify that species have at least one checklist for making a graph
species_checklists_count <- sapply(seq_len(nrow(species_data)), function(i) {
  ### initialization
  checklists_starting_year <- species_data$checklists_starting_year[i]
  sum((record_data$species_scientific_name ==
       species_data$species_scientific_name[i]) &
      (record_data$year >= checklists_starting_year) &
      (record_data$is_checklist))
})
species_invalid_settings <- (species_checklists_count == 0) &
                            (grepl("1", species_data$graphs) |
                             grepl("3", species_data$graphs) |
                             grepl("6", species_data$graphs))
if (any(species_invalid_settings)) {
  stop(paste("The following species do not have a single complete checklist",
             "after the specified starting year but require graphs:",
       paste(species_data$species_scientific_name[species_invalid_settings],
                   collapse = ", ")))
}

## subset data if required
if (!identical(parameters$number_species, "all"))
  species_data <- species_data[seq_len(parameters$number_species), ,
                               drop = FALSE]

## extract spatial attributes
locality_data <- record_data[!duplicated(record_data$locality), "locality"]

### extract elevation data
elevation_data <- raster::crop(elevation_data,
  raster::extent(as(sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_buffer(
    study_area_simple_data, 200000), 4326))), "Spatial")))
locality_pts <- as(locality_data, "Spatial")
elevation_data <- raster::projectRaster(elevation_data, method = "bilinear",
                                        crs = locality_pts@proj4string)
elevation_data[raster::Which(elevation_data < 0)] <- 0
elevation_data[raster::Which(is.na(elevation_data))] <- 0
locality_data$elevation <- raster::extract(elevation_data, locality_data)
locality_data$elevation[!is.finite(locality_data$elevation)] <- 0
rm(locality_pts, elevation_data)

### extract vegetation data
locality_pos <- max.col(cbind(as.matrix(sf::st_intersects(locality_data,
                                                          vegetation_data)),
                              FALSE), ties.method = "last")
locality_data$vegetation_class <- vegetation_data[[
  parameters$vegetation$class_column_name]][locality_pos]
assertthat::assert_that(assertthat::noNA(locality_data$vegetation_class),
  msg = "all eBird records must overlap with vegetation data")

## extract grid cell id
grid_pos <- max.col(cbind(as.matrix(sf::st_intersects(locality_data,
                                                      grid_data)),
                          FALSE), ties.method = "last")
locality_data$grid_id <- grid_data$id[grid_pos]
locality_data$grid_type <- grid_data$type[grid_pos]
assertthat::assert_that(
  assertthat::noNA(locality_data$grid_id),
  assertthat::noNA(locality_data$grid_type),
  msg = "something went wrong subsetting records...")

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

tictoc::toc(log = TRUE, quiet = TRUE)

# Set up variables for caching
tictoc::tic("Set up variables for caching")

## graphs hashes
tmp_hash <- digest::digest(list(record_data, parameters$graphs))
tmp_df <- dplyr::select(species_data, graphs, checklists_starting_year,
                        records_starting_year, distribution)
species_data$graphs_hash <- plyr::laply(
  seq_len(nrow(species_data)), function(i) {
   digest::digest(list(tmp_df[i, ], tmp_hash))
})

## tables hashes
tmp_hash <- digest::digest(list(record_data, grid_data))
tmp_df <- dplyr::select(species_data, graphs, checklists_starting_year,
                        records_starting_year, distribution, iucn_threat_status,
                        national_threat_status, qld_threat_status)
species_data$table_hash <- plyr::laply(
  seq_len(nrow(species_data)), function(i) {
    digest::digest(list(tmp_df[i, ], tmp_hash))
})

## maps hashes
tmp_hash <- digest::digest(list(record_data, grid_data, parameters$maps))
tmp_df <- dplyr::select(species_data, maps, checklists_starting_year,
                        records_starting_year, distribution)
species_data$maps_hash <- plyr::laply(
  seq_len(nrow(species_data)), function(i) {
    digest::digest(list(tmp_df[i, ], tmp_hash))
})

## widget hashes
tmp_hash <- digest::digest(list(record_data, grid_data, parameters$maps))
tmp_df <- dplyr::select(species_data, maps, checklists_starting_year,
                        records_starting_year, distribution)
species_data$widget_hash <- plyr::laply(
  seq_len(nrow(species_data)), function(i) {
    digest::digest(list(tmp_df[i, ], tmp_hash))
})

## surveyor sheet hashes
tmp_hash <- digest::digest(list(record_data, grid_data, species_data,
              parameters$surveyor_sheets,
              parameters$grid_resolution,
              readLines("templates/surveyor-sheet.Rmd"),
              readLines("templates/surveyor-sheet-preamble.tex")))
grid_data$sheet_hash <- plyr::laply(
  seq_len(nrow(grid_data)), function(i) {
    digest::digest(list(grid_data[i, ], tmp_hash))
})

tictoc::toc(log = TRUE, quiet = TRUE)

# Exports
tictoc::tic("Exports")

## spawn cluster workers
is_parallel <- isTRUE(parameters$threads > 1)
if (is_parallel) {
  cl <- parallel::makeCluster(parameters$threads, type = "PSOCK")
  parallel::clusterEvalQ(cl, {library(raster); library(dplyr); library(sf);
                              library(patchwork)})
  parallel::clusterExport(cl, envir = environment(),
                           c("species_data", "record_data", "grid_data",
                             "study_area_data", "study_area_simple_data",
                             "land_data", "parameters",
                             "species_graph", "species_map", "species_table",
                             "species_widget", "color_numeric_palette", "ymax",
                             "breaks", "addLegend_custom", "file_names",
                             "add_reporting_rate_columns",
                             "add_detection_columns",
                             "find_observer_name", "grid_summary_table",
                             "grid_checklist_table", "grid_map",
                             "locations_data", "latest_checklist_month_year"))
  doParallel::registerDoParallel(cl)
}

## create surveyor sheet for each grid cell
tictoc::tic("Surveyor sheets")
message("starting surveyor sheets...")
### determine which grids for which to make surveyor sheets
grid_indices <- which(grid_data$type == "land")
if (!identical(parameters$number_surveyor_sheets, "all"))
  grid_indices <- grid_indices[seq_len(parameters$number_surveyor_sheets)]
### make surveyor sheets
result <- plyr::laply(grid_indices, .parallel = is_parallel,
                      function(i) {
  # display progress
  message("  ", grid_data$id[i])
  # create file names
  asset_path <- paste0("assets/surveyor-sheets/grid-", grid_data$id[i], "-",
                       gsub(" ", "-", tolower(grid_data$name[i]), fixed = TRUE),
                       ".pdf")
  hash_path <- paste0("assets/surveyor-sheets/grid-", grid_data$id[i], "-",
                      gsub(" ", "-", tolower(grid_data$name[i]), fixed = TRUE),
                      ".hash")
  # check if processing needed if hash file already exists
  if (file.exists(hash_path)) {
    # load hash
    curr_hash <- readLines(hash_path)
    # check if hash is the same the hash for this species
    if (identical(grid_data$sheet_hash[i], curr_hash)) {
      # if the hash is the same then skip this species
      message("    reusing cache")
      return(TRUE)
    }
  }
  # create image for grid
  grid_map_data <- grid_map(
    grid_data$id[i], grid_data, locations_data,
    study_area_data,
    parameters$grid_resolution,
    parameters$surveyor_sheets$maps$zoom_level,
    as.character(grid_data$map_type[i]),
    parameters$surveyor_sheets$maps$grid_line_color,
    parameters$surveyor_sheets$maps$grid_line_width,
    parameters$surveyor_sheets$maps$study_area_line_color,
    parameters$surveyor_sheets$maps$study_area_line_width)
  grid_map_path <- tempfile(fileext = ".jpg")
  ggplot2::ggsave(grid_map_data, filename = grid_map_path,
                  width = parameters$surveyor_sheets$maps$width,
                  height = parameters$surveyor_sheets$maps$height)
  # create checklist table for grid
  grid_summary <- grid_summary_table(grid_data$id[i], grid_data, species_data,
                                     record_data)
  # create summary table for grid
  grid_checklist <- grid_checklist_table(grid_data$id[i], species_data,
                                         record_data)
  # create temporary intermediates
  int_dir <- tempfile()
  dir.create(int_dir, showWarnings = FALSE, recursive = TRUE)
  # make rmarkdown document
  rmarkdown::render("templates/surveyor-sheet.Rmd",
    output_file = basename(asset_path),
    intermediates_dir = int_dir,
    output_dir = dirname(asset_path),
    clean = TRUE,
    params = list(grid_id = grid_data$id[i],
                  grid_name = grid_data$name[i],
                  grid_date = latest_checklist_month_year,
                  grid_checklist_target = grid_data$checklist_target[i],
                  grid_minute_target = grid_data$minute_target[i],
                  grid_km_target = grid_data$km_target[i],
                  grid_map = grid_map_path,
                  grid_summary = grid_summary,
                  grid_checklist = grid_checklist,
                  grid_description = grid_data$description[i],
                  grid_checklist_number_of_pages =
                    parameters$surveyor_sheets$checklist$number_of_pages,
                  grid_checklist_vertical_spacing =
                    parameters$surveyor_sheets$checklist$vertical_spacing,
                  grid_checklist_left_margin =
                    parameters$surveyor_sheets$checklist$left_margin,
                  grid_checklist_right_margin =
                    parameters$surveyor_sheets$checklist$right_margin))
  # cleanup temp file
  unlink(grid_map_path)
  unlink(int_dir, recursive = TRUE, force = TRUE)
  # save hash
  writeLines(grid_data$sheet_hash[i], hash_path)
  # return logical indicating success
  TRUE
})

tictoc::toc(log = TRUE, quiet = TRUE)

## create graphs for each species
tictoc::tic("Graphs")
message("starting graphs...")
result <- plyr::laply(seq_len(nrow(species_data)), .parallel = is_parallel,
                      function(i) {
  # display progress
  message("  ", species_data$species_scientific_name[i])
  # create file names
  asset_path <- paste0("assets/graphs/", file_names[i], ".png")
  hash_path <- paste0("assets/graphs/", file_names[i], ".hash")
  # check if processing needed if hash file already exists
  if (file.exists(hash_path)) {
    # load hash
    curr_hash <- readLines(hash_path)
    # check if hash is the same the hash for this species
    if (identical(species_data$graphs_hash[i], curr_hash)) {
      # if the hash is the same then skip this species
      message("    reusing cache")
      return(TRUE)
    }
  }
  # create species graphs
  p <- species_graph(species_data$species_scientific_name[i], species_data,
                     record_data)
  # save graphs
  if (!is.null(p)) {
    n <- as.character(stringr::str_count(species_data$graphs[i], "_") + 1)
    ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".png"), p,
                    width = parameters$graphs$size[[n]]$width,
                    height = parameters$graphs$size[[n]]$height,
                    units = "in")
    ggplot2::ggsave(paste0("assets/graphs/", file_names[i], ".jpeg"), p,
                    width = parameters$graphs$size[[n]]$width,
                    height = parameters$graphs$size[[n]]$height,
                    units = "in")
    system(paste0("jpegoptim ", paste0("assets/graphs/",
                  file_names[i], ".jpeg")))
  }
  # save hash
  writeLines(species_data$graphs_hash[i], hash_path)
  # return logical indicating success
  TRUE
})

tictoc::toc(log = TRUE, quiet = TRUE)

## create tables
tictoc::tic("Tables")
message("starting tables...")
result <- plyr::laply(seq_len(nrow(species_data)), .parallel = is_parallel,
                      function(i) {
  # display progress
  message("  ", species_data$species_scientific_name[i])
  # create file names
  asset_path <- paste0("assets/tables/", file_names[i], ".rds")
  hash_path <- paste0("assets/tables/", file_names[i], ".hash")
  # check if processing needed if hash file already exists
  if (file.exists(hash_path)) {
    # load hash
    curr_hash <- readLines(hash_path)
    # check if hash is the same the hash for this species
    if (identical(species_data$table_hash[i], curr_hash)) {
      # if the hash is the same then skip this species
      message("    reusing cache")
      return(TRUE)
    }
  }
  # create species tables
  p <- species_table(species_data$species_scientific_name[i], species_data,
                     record_data)
  # save tables
  saveRDS(p, asset_path, compress = "xz")
  # save hash
  writeLines(species_data$table_hash[i], hash_path)
  # return logical indicating success
  TRUE
})

tictoc::toc(log = TRUE, quiet = TRUE)

## create interactive maps for each species
tictoc::tic("Widgets")
message("starting widgets...")
result <- plyr::laply(seq_len(nrow(species_data)), .parallel = is_parallel,
                      function(i) {
  # display progress
  message("  ", species_data$species_scientific_name[i])
  # create file names
  asset_path <- paste0("assets/widgets/", file_names[i], ".rds")
  hash_path <- paste0("assets/widgets/", file_names[i], ".hash")
  # check if processing needed if hash file already exists
  if (file.exists(hash_path)) {
    # load hash
    curr_hash <- readLines(hash_path)
    # check if hash is the same the hash for this species
    if (identical(species_data$widget_hash[i], curr_hash)) {
      # if the hash is the same then skip this species
      message("    reusing cache")
      return(TRUE)
    }
  }
  # create species widget
  p <- species_widget(species_data$species_scientific_name[i], species_data,
                      record_data, grid_data, study_area_simple_data,
                      parameters$maps$minimum_required_checklists,
                      parameters$maps$minimum_required_events,
                      parameters$maps$colors)
  # save widget
  if (!is.null(p)) {
    saveRDS(p, asset_path, compress = "xz")
  }
  # save hash
  writeLines(species_data$widget_hash[i], hash_path)
  # return logical indicating success
  TRUE
})

tictoc::toc(log = TRUE, quiet = TRUE)

## create static maps for each species
tictoc::tic("Maps")
message("starting maps...")
result <- plyr::laply(seq_len(nrow(species_data)), .parallel = is_parallel,
                      function(i) {
  # display progress
  message("  ", species_data$species_scientific_name[i])
  # create file names
  asset_path <- paste0("assets/maps/", file_names[i], ".png")
  hash_path <- paste0("assets/maps/", file_names[i], ".hash")
  # check if processing needed if hash file already exists
  if (file.exists(hash_path)) {
    # load hash
    curr_hash <- readLines(hash_path)
    # check if hash is the same the hash for this species
    if (identical(species_data$maps_hash[i], curr_hash)) {
      # if the hash is the same then skip this species
      message("    reusing cache")
      return(TRUE)
    }
  }
  # create species maps
  p <- species_map(species_data$species_scientific_name[i], species_data,
                   record_data, grid_data, land_data, study_area_simple_data,
                   parameters$maps$minimum_required_checklists,
                   parameters$maps$minimum_required_events,
                   parameters$maps$colors)
  # save species maps
  if (!is.null(p)) {
    n <- as.character(stringr::str_count(species_data$maps[i], "_") + 1)
    ggplot2::ggsave(paste0("assets/maps/", file_names[i], ".png"), p,
                    width = parameters$maps$size[[n]]$width,
                    height = parameters$maps$size[[n]]$height,
                    units = "in")
  }
  # save hash
  writeLines(species_data$maps_hash[i], hash_path)
  # return logical indicating success
  TRUE
})

tictoc::toc(log = TRUE, quiet = TRUE)

## cleanup
if (is_parallel) {
  doParallel::stopImplicitCluster()
  cl <- parallel::stopCluster(cl)
}

tictoc::toc(log = TRUE, quiet = TRUE)

# finish timer
tictoc::toc(log = TRUE, quiet = TRUE)

# print total runtime
tictoclog <- tictoc::tic.log(format = TRUE
writeLines(unlist(tictoglog)))
