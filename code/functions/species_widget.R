#' Create species widget
#'
#' @param x \code{character} scientific name of species.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   data indicating which maps should be created. The argument to
#'   \code{species_data} must have the columns
#'   \code{"species_scientific_name"} and \code{"maps"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"season"}, \code{"is_checklist"},
#'   \code{"is_after_start_year"}, \code{"is_fully_sampled_year"}, and
#'   \code{"maps"}.
#'
#' @param grid_data \code{\link[raster]{RasterLayer} object containing the grid
#'   cells for displaying data on the map.
#'
#' @param study_area_data \code{sf} object showing the study area.
#'
#' @return interactive widget.
species_widget <- function(x, species_data, record_data, grid_data,
                           study_area_data) {
  # Initialization
  ## coerce grid data NAs to zeros
  grid_data[is.na(grid_data)] <- 0
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## determine which maps to create
  map_numbers <- species_data$maps[species_data$species_scientific_name == x]
  map_numbers <- as.numeric(strsplit(map_numbers, "_")[[1]])
  if (min(map_numbers, na.rm = TRUE) < 1 ||
      max(map_numbers, na.rm = TRUE) > 5 ||
      any(is.na(map_numbers)))
    stop(paste0("processing ", x, "\ndata in maps column must contain ",
                "integers between 1 and 5 separated by underscores ",
                "(e.g. 1_2_3_4_5"))
  if (length(map_numbers) == 0)
    stop(paste("processing ", x, "\ndata in maps column must specify at least",
               "one map"))
  ## create check list data with all check lists
  chk_data <- record_data[record_data$is_checklist &
                          record_data$is_after_start_year &
                          record_data$is_fully_sampled_year,
                          c("species_scientific_name", "season", "event"),
                          drop = FALSE]
  ## create check list data with check list for species
  spp_data <- chk_data[chk_data$species_scientific_name == x ,
                       c("species_scientific_name", "season", "event"),
                       drop = FALSE]
  ## remove duplicate data entries
  chk_data <- chk_data[!duplicated(chk_data$event), , drop = FALSE]
  spp_data <- spp_data[!duplicated(spp_data$event), , drop = FALSE]
  # Preliminary processing
  ## create raster stack to store frequency data
  layer_names <- c("annual", "summer", "autumn", "winter", "spring")
  grid_data <- raster::stack(grid_data, grid_data, grid_data, grid_data,
                             grid_data)
  names(grid_data) <- layer_names
  ## calculate frequency of checklists in grid cells
  spp_cells <- raster::extract(grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(grid_data[[1]],
                               as(chk_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  ### seasons calculations
  for (l in layer_names) {
    # extract grid cells
    if (l == "annual") {
      spp_tbl <- as.data.frame(table(spp_cells))
      chk_tbl <- as.data.frame(table(chk_cells))
    } else {
      spp_tbl <- as.data.frame(table(spp_cells[spp_data$season == l]))
      chk_tbl <- as.data.frame(table(chk_cells[chk_data$season == l]))
    }
    # skip if no checklists at all in this season for this species
    if (nrow(chk_tbl) > 0) {
      chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
      if (nrow(spp_tbl) == 0) {
        # assign zeros to cells with checklists for other species
        grid_data[[l]][chk_tbl[[1]]] <- NA_real_
      } else {
        # assign total number of check lists to grid cells
        chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
        grid_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
        # calculate reporting rate for cells with checklists
        spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
        grid_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
                                        grid_data[[l]][spp_tbl[[1]]]
        # assign zeros to cells with checklists where this species wasn't
        # detected
        grid_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- NA_real_
      }
    }
  }
  ## convert proportions to percentages
  grid_data <- grid_data * 100
  ## create group names
  group_names <- c("Annual", "Summer", "Autumn", "Winter", "Spring")
  names(grid_data) <- group_names
  ## subset data to specified map numbers
  grid_data <- grid_data[[map_numbers]]
  # main processing
  ## initialize leaflet map
  l <- leaflet::leaflet()
  ## create palettes
  grid_data2 <- grid_data
  for (i in seq_len(raster::nlayers(grid_data2)))
    grid_data2[[i]][grid_data2[[i]] < 1e-10] <- NA_real_
  palette <- color_numeric_palette("viridis",
    range(c(c(raster::cellStats(grid_data2, "min")),
            c(raster::cellStats(grid_data2, "max")))),
    na.color = "grey70", outside.color = "transparent")
  palette_rev <- color_numeric_palette("viridis",
    range(c(c(raster::cellStats(grid_data2, "min")),
            c(raster::cellStats(grid_data2, "max")))),
    na.color = "grey70", outside.color = "transparent", reverse = TRUE)
  ## add tiles
  l <- leaflet::addProviderTiles(l, "Esri.WorldGrayCanvas", group = "Thematic")
  ## add rasters
  for (i in names(grid_data))
    l <- leaflet::addRasterImage(l, x = grid_data[[i]], project = FALSE,
                                 colors = palette, opacity = 0.6, group = i)
  ## add polygons
  l <- leaflet::addPolygons(l, color = "black", fillColor = "transparent",
                            data = as(sf::st_transform(study_area_data, 4326),
                                      "Spatial"),
                            group = "Brisbane extent")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    baseGroups = group_names,
    overlayGroups = "Brisbane extent",
    options = leaflet::layersControlOptions(collapsed = FALSE))
  ## add legend
  l <- leaflet::addLegend(l, pal = palette_rev, opacity = 1,
                          values = na.omit(c(raster::values(grid_data))),
                          title = "Rate (%)",
                          position = "topright",
                          labFormat = leaflet::labelFormat(transform = rev))
  # exports
  ## return widget
  l
}
