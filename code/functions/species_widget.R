#' Create species widget
#'
#' @param x \code{character} scientific name of species.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"} and \code{"season"}.
#'
#' @param grid_data \code{sf} object containing the grid cells for displaying
#'   on the map.
#'
#' @return interactive widget.
species_widget <- function(x, record_data, grid_data) {
  # initialization
  ## subset record data
  x <- x[x$species_scientific_name == x, , drop = FALSE]
  ## calculate frequency of records in grid cells
  ovr <- as.matrix(sf::st_intersects(x[!is.na(x$season), ], grid_data,
                   sparse = FALSE))
  grid_data$total_freq <- colSums(ovr)
  grid_data$summer_freq <- colSums(ovr[x$season == "spring", , drop = FALSE])
  grid_data$autumn_freq <- colSums(ovr[x$season == "autumn", , drop = FALSE])
  grid_data$winter_freq <- colSums(ovr[x$season == "winter", , drop = FALSE])
  grid_data$spring_freq <- colSums(ovr[x$season == "spring", , drop = FALSE])
  ## store column names
  column_names <- c("total_freq", "summer_freq", "autumn_freq", "winter_freq",
                    "spring_freq")
  ## create group names
  group_names <- c("Annual", "Summer", "Autumn", "Winter", "Spring")
  names(group_names) <- column_names
  # main processing
  ## initialize leaflet map
  p <- leaflet::leaflet()
  ## create palettes
  pal_list <- lapply(column_names, function(i)
    leaflet::colorNumeric("viridis", grid_data[[i]],
                          na.color = "transparent"))
  names(pal_list) <- column_names
  ## add tiles
  p <- leaflet::addProviderTiles(p, "Esri.WorldImagery")
  ## add polygons
  for (i in column_names)
    p <- leaflet::addPolygons(p, data = grid_data[[i]],
                              fillColor = pal_list[[i]](grid_data[[i]]),
                              fillOpacity = 0.6, color = "#333333",
                              weight = 2.5, opacity = 0.8,
                              group = group_names[[i]])
  ## add legends
  for (i in column_names)
    p <- leaflet::addLegend(p, pal = pal_list[[i]],
                            values = grid_data[[i]],
                            title = group_names[[i]],
                            position = "topleft")
  ## add points
  p <- leaflet::addMarkers(p, data = x, group = "Sightings",
    clusterOptions = leaflet::markerClusterOptions()) %>%
  ## add layer control
  p <- leaflet::addLayersControl(p,
    baseGroups = group_names, overlayGroups = "Sightings",
    options = leaflet::layersControlOptions(collapsed = FALSE))
  # exports
  ## return widget
  p
}
