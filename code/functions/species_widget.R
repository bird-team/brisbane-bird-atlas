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
#' @param minimum_required_checklists \code{numeric} number of checklists
#'   required for a grid cell to be adequately sampled for reporting rates.
#'
#' @param minimum_required_events \code{numeric} number of sampling events
#'   required for a grid cell to be adequately sampled for range estimates.
#'
#' @return interactive widget.
species_widget <- function(x, species_data, record_data, grid_data,
                           study_area_data, minimum_required_checklists,
                           minimum_required_events) {
  # Initialization
  tmp_data <- grid_data
  grid_data[is.na(tmp_data)] <- 0
  grid_data[!is.na(tmp_data)] <- NA_real_
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## determine which maps to create
  map_numbers <- species_data$maps[species_data$species_scientific_name == x]
  map_numbers <- as.numeric(strsplit(map_numbers, "_")[[1]])
  if (min(map_numbers, na.rm = TRUE) < 1 ||
      max(map_numbers, na.rm = TRUE) > 6 ||
      any(is.na(map_numbers)))
    stop(paste0("processing ", x, "\ndata in maps column must contain ",
                "integers between 1 and 6 separated by underscores ",
                "(e.g. 1_2_3_4_5_6"))
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
  layer_names <- c("all_year", "summer", "autumn", "winter", "spring")
  rate_data <- grid_data[[rep(1, 5)]]
  names(rate_data) <- layer_names
  ## calculate frequency of checklists in grid cells
  spp_cells <- raster::extract(grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(grid_data[[1]],
                               as(chk_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  for (l in layer_names) {
    # extract grid cells
    if (l == "all_year") {
      spp_tbl <- as.data.frame(table(spp_cells))
      chk_tbl <- as.data.frame(table(chk_cells))
    } else {
      spp_tbl <- as.data.frame(table(spp_cells[spp_data$season == l]))
      chk_tbl <- as.data.frame(table(chk_cells[chk_data$season == l]))
    }
    # coerce factors to integers (safely)
    chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
    spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
    # identify cells with inadequate numbers of checklists
    poorly_sampled <- chk_tbl[[2]] < minimum_required_checklists
    # set poorly sampled cells as NA in rate_data[[l]]
    rate_data[[l]][chk_tbl[[1]][poorly_sampled]] <- NA_real_
    # remove cells with inadequate numbers of checklists
    chk_tbl <- chk_tbl[!poorly_sampled, , drop = FALSE]
    spp_tbl <- spp_tbl[spp_tbl[[1]] %in% chk_tbl[[1]], , drop = FALSE]
    # skip if no checklists at all in this season for this species
    if (nrow(chk_tbl) > 0) {
      if (nrow(spp_tbl) == 0) {
        # assign zeros to calls with checklists for other species
        rate_data[[l]][chk_tbl[[1]]] <- 0
      } else {
        # assign total number of check lists to grid cells
        rate_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
        # calculate reporting rate for cells with checklists
        rate_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
                                        rate_data[[l]][spp_tbl[[1]]]
        # assign zeros to cells with checklists where this species wasn't
        # detected
        rate_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- 0
      }
    }
  }
  ## convert proportions to percentages
  rate_data <- rate_data * 100
  ## create detection data
  detection_data <- grid_data[[1]]
  chk_cells2 <- raster::extract(grid_data[[1]],
    as(record_data[!duplicated(record_data$event), "season"], "Spatial"),
    cellnumbers = TRUE)[, 1]
  spp_cells2 <- raster::extract(grid_data[[1]],
    as(record_data[record_data$species_scientific_name == x, "season"],
       "Spatial"), cellnumbers = TRUE)[, 1]
  # coerce to table
  chk_tbl2 <- as.data.frame(table(chk_cells2))
  spp_tbl2 <- as.data.frame(table(spp_cells2))
  # coerce factors to integers (safely)
  chk_tbl2[[1]] <- as.integer(as.character(chk_tbl2[[1]]))
  spp_tbl2[[1]] <- as.integer(as.character(spp_tbl2[[1]]))
  # identify cells with inadequate numbers of checklists
  poorly_sampled2 <- chk_tbl2[[2]] < minimum_required_events
  # set poorly sampled cells as NA in detection_data[[l]]
  detection_data[chk_tbl2[[1]][poorly_sampled2]] <- NA_real_
  # remove cells with inadequate numbers of checklists
  chk_tbl2 <- chk_tbl2[!poorly_sampled2, , drop = FALSE]
  spp_tbl2 <- spp_tbl2[spp_tbl2[[1]] %in% chk_tbl2[[1]], , drop = FALSE]
  # assign values
  detection_data[spp_tbl2[[1]]] <- 1
  detection_data[setdiff(chk_tbl2[[1]], spp_tbl2[[1]])] <- 0
  ## create group names
  group_names <- c("All.year", "Summer", "Autumn", "Winter", "Spring",
                   "Detection")
  complete_data <- raster::addLayer(rate_data, detection_data)
  names(complete_data) <- group_names
  ## subset data to specified map numbers
  complete_data <- complete_data[[map_numbers]]
  ## calculate initial view settings
  inital_view <- sf::st_transform(sf::st_centroid(study_area_data), 4326)
  inital_view <- c(as(inital_view, "Spatial")@coords)
  # main processing
  ## initialize leaflet map
  l <- leaflet::leaflet()
  l <- leaflet::setView(l, lng = inital_view[[1]], lat = inital_view[[2]],
                        zoom = 10)
  ## create palettes
  br <- pretty(na.omit(c(raster::values(rate_data))))
  palette <- color_numeric_palette("viridis", domain = range(br),
                                   na.color = "#b3b3b3",
                                   zero.color = "transparent")
  palette_rev <- color_numeric_palette("viridis", domain = range(br),
                                       na.color = "#b3b3b3",
                                       reverse = TRUE)
  bin_palette <- function(x) {
    cols <- rep("#ff0000", length(x))
    cols[is.na(x)] <- "#b3b3b3"
    cols[which(abs(x) < 1e-100)] <- "transparent"
    cols
  }
  ## add tiles
  l <- leaflet::addProviderTiles(l, "Esri.WorldGrayCanvas", group = "Thematic")
  ## add rasters
  for (i in names(complete_data)) {
    curr_palette <- palette
    if (i == "Detection") curr_palette <- bin_palette
    l <- leaflet::addRasterImage(l, x = complete_data[[i]], project = FALSE,
                                 colors = curr_palette, opacity = 0.6,
                                 group = gsub(".", " ", i, fixed = TRUE))
  }
  ## add polygons
  l <- leaflet::addPolygons(l, color = "black", fillColor = "transparent",
                            weight = 2.5, smoothFactor = 0.5,
                            data = as(sf::st_transform(study_area_data, 4326),
                                      "Spatial"),
                            group = "Brisbane extent")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    baseGroups = gsub(".", " ", names(complete_data), fixed = TRUE),
    overlayGroups = "Brisbane extent",
    options = leaflet::layersControlOptions(collapsed = FALSE))
  ## add legend
  if (6 %in% map_numbers) {
    l <- leaflet::addLegend(l, opacity = 1, colors = c("#fff", "#b3b3b3", "#f00"),
                            labels = c("0% Rate", "Unknown", "Detected"),
                            title = NULL, position = "topright")
  } else {
    l <- leaflet::addLegend(l, opacity = 1, colors = c("#fff", "#b3b3b3"),
                            labels = c("0% Rate", "Unknown"),
                            title = NULL, position = "topright")
  }
  if (any(seq_len(5) %in% map_numbers)) {
    l <- addLegend_custom(l, pal = palette_rev, opacity = 1, bins = br,
                            values = seq(min(br), max(br), length.out = 100),
                            title = "Rate (%)", position = "topright",
                            labFormat = leaflet::labelFormat(transform = rev))
  }
  # exports
  ## return widget
  l
}
