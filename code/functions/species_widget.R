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
#'   \code{"year"}, \code{"is_fully_sampled_year"}, and
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
#' @param colors \code{character} color codes for making color ramps.
#'
#' @return interactive widget.
species_widget <- function(x, species_data, record_data, grid_data,
                           study_area_data, minimum_required_checklists,
                           minimum_required_events, colors) {
  # Initialization
  ## coerce record data to data.frame
  record_data <- as.data.frame(record_data) %>% dplyr::select(-geometry)
  ## determine if grid should be land, marine, or both
  spp_index <- which(species_data$species_scientific_name == x)
  spp_type <- species_data$distribution[spp_index]
  ## determine which maps to create
  map_numbers <- species_data$maps[spp_index]
  if (is.na(map_numbers)) return(NULL)
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
  ## calculate initial view settings
  if (spp_type == "land") {
    inital_view <- study_area_data %>%
                   filter(name == "land") %>%
                   sf::st_union() %>%
                   sf::st_centroid() %>%
                   sf::st_transform(4326)
    inital_view <- c(as(inital_view, "Spatial")@coords)
    inital_view <- c(inital_view, 10)
  } else {
    inital_view <- NULL
  }
  ## set up grid
  ### subset cells that are relevant for species
  if (spp_type == "land") {
    grid_data <- grid_data %>%
                 filter(type == "land")
  } else if (spp_type == "marine") {
   grid_data <- grid_data %>%
                filter(type == "marine")
  }
  ## set up study area data
  if (spp_type == "land") {
    study_area_data <- study_area_data %>%
                       filter(name == "land")
  } else if (spp_type == "marine") {
    study_area_data <- study_area_data %>%
                       filter(name == "marine")
  }
  ## remove "name" column in data
  ## (this is to ensure the study area data appears in all panels when
  ## faceting data in the ggplot2 plots later on)
  study_area_data$name <- NULL
  land_data$name <- NULL
  ## determine starting years for records and checklists
  checklists_starting_year <- species_data$checklists_starting_year[spp_index]
  records_starting_year <- species_data$records_starting_year[spp_index]
  # Preliminary processing
  ## add reporting rate columns
  grid_data <- add_reporting_rate_columns(x, grid_data, record_data,
                                         checklists_starting_year,
                                         minimum_required_checklists)
  ## add detection columns
  grid_data <- add_detection_columns(x, grid_data, record_data,
                                     records_starting_year,
                                     minimum_required_events)
  grid_data <- grid_data %>%
               as("Spatial")
  names(grid_data@data) <- gsub(".", " ", names(grid_data@data), fixed = TRUE)
  plot_names <- c("All year", "Summer", "Autumn", "Winter", "Spring",
                  "Detection")[map_numbers]
  ## rasterize the grid data
  grid_raster_raw_data <- raster::raster(
    xmn = raster::xmin(grid_data), ymn = raster::ymin(grid_data),
    xmx = raster::xmax(grid_data), ymx = raster::ymax(grid_data),
    res = sqrt(rgeos::gArea(grid_data[1, ])),
    crs = grid_data@proj4string)
  grid_raster_raw_data <- raster::setValues(grid_raster_raw_data, 0)
  ## convert grid_data to points for easier overlay with raster data
  grid_pts_data <- sp::SpatialPointsDataFrame(
                     rgeos::gCentroid(grid_data, byid = TRUE)@coords,
                     data = data.frame(id = seq_len(nrow(grid_data))),
                     proj4string = grid_data@proj4string)
  grid_pts_data@data <- grid_data@data
  grid_pts_data@data$cell <- raster::cellFromXY(grid_raster_raw_data,
                                                grid_pts_data@coords)
  # main processing
  ## initialize leaflet map
  l <- leaflet::leaflet()
  ## set starting view
  if (!is.null(inital_view)) {
    l <- leaflet::setView(l, lng = inital_view[[1]], lat = inital_view[[2]],
                          zoom = inital_view[[3]])
  }
  ## create palettes
  br <- pretty(na.omit(unlist(
    grid_data@data[, plot_names[seq_len(5)]], use.names = FALSE)))
  palette <- color_numeric_palette(colors, domain = range(br),
                                   na.color = "#b3b3b3",
                                   zero.color = "transparent")
  palette_rev <- color_numeric_palette(colors, domain = range(br),
                                       na.color = "#b3b3b3",
                                       reverse = TRUE)
  bin_palette <- function(x) {
    cols <- rep("#ff0000", length(x))
    cols[is.na(x)] <- "#b3b3b3"
    cols[which(abs(x) < 1e-100)] <- "#00000000"
    cols
  }
  ## add tiles
  l <- leaflet::addProviderTiles(l, "Esri.WorldGrayCanvas", group = "Thematic")
  ## add reporting rate and detection maps (as required)
  for (i in plot_names) {
    curr_palette <- palette
    if (i == "Detection") curr_palette <- bin_palette
    r <- grid_raster_raw_data
    r[grid_pts_data$cell] <- grid_pts_data@data[[i]]
    l <- leaflet::addRasterImage(l, r, project = FALSE,
                                 colors = curr_palette, opacity = 0.6,
                                 group = i)
  }
  ## add Brisbane extent
  l <- leaflet::addPolygons(l, color = "black", fillColor = "transparent",
                            weight = 2.5, smoothFactor = 0.4,
                            data = as(sf::st_transform(study_area_data, 4326),
                                      "Spatial"),
                            group = "Brisbane extent")
  ## add sampling grid
  l  <- leaflet::addPolygons(l, color = "black", fillColor = "transparent",
                            weight = 2.5,
                            data = sp::spTransform(grid_data,
                                                   sp::CRS("+init=epsg:4326")),
                            popup = htmltools::htmlEscape(grid_data$name),
                            group = "Grid")
  l <- leaflet::hideGroup(l, "Grid")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    baseGroups = plot_names,
    overlayGroups = c("Brisbane extent", "Grid"),
    options = leaflet::layersControlOptions(collapsed = FALSE))
  ## add legend
  if (6 %in% map_numbers) {
    l <- leaflet::addLegend(l, opacity = 1,
                            colors = c("#fff", "#b3b3b3", "#f00"),
                            labels = c("0% Rate", "Unknown", "Detected"),
                            title = NULL, position = "topright")
  } else {
    l <- leaflet::addLegend(l, opacity = 1, colors = c("#fff", "#b3b3b3"),
                            labels = c("0% Rate", "Unknown"),
                            title = NULL, position = "topright")
  }
  if (any(seq_len(5) %in% map_numbers) &
      (max(unlist(grid_data@data[, plot_names[seq_len(5)]],
                  use.names = FALSE), na.rm = TRUE) > 1e-10)) {
    l <- addLegend_custom(l, pal = palette_rev, opacity = 1, bins = br,
                            values = seq(min(br), max(br), length.out = 100),
                            title = "Rate (%)", position = "topright",
                            labFormat = leaflet::labelFormat(transform = rev))
  }
  # exports
  ## return widget
  l
}
