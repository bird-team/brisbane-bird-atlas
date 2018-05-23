#' Plot species maps
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
#' @param land_data \code{sf} object showing the land masses. This
#'   is not used if the argument to \code{interactive} is \code{FALSE}
#'
#' @param interactive should an interactive map be produced? The default
#'   argument is \code{TRUE} if \code{isTRUE(knitr:::is_html_output())},
#'   otherwise the default argument is \code{FALSE}.
#'
#' @return An interactive \pkg{leaflet} map or a static \pkg{ggplot2} map.
plot_species_maps <- function(x, record_data, grid_data, land_data,
                              interactive = isTRUE(knitr:::is_html_output())) {
  # initialization
  ## subset record data
  x <- x[x$species_scientific_name == x, , drop = FALSE]

  ### calculate frequency of records in grid cells
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
  if (interactive) {
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
  } else {
    ## set boundary box data
    bb <- sf::st_bbox(grid_data)
    ## format data for plotting
    plot_data <- sf::st_sf(
      freq = c(as.matrix(as.data.frame(x)[, column_names[-1]])),
      season = rep(group_names[-1], each = nrow(grid_data)),
      geometry  = sf::st_geometry(grid_data)[rep(seq_len(nrow(grid_data)), 4)])
    ## create plot
    p <- ggplot2::ggplot() +
         ggplot2::geom_sf(data = land_data, color = "#333333",
                          fill = "#333333") +
         ggplot2::geom_sf(data = plot_data, ggplot2::aes(fill = freq)) +
         ggplot2::coord_equal() +
         ggplot2::coord_cartesian(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
         ggplot2::theme(
           axis.ticks = ggplot2::element_blank(),
           axis.text = ggplot2::element_blank(),
           axis.title = ggplot2::element_blank(),
           axis.line = ggplot2::element_blank(),
           axis.ticks.length = ggplot2::unit(0, "null"),
           axis.ticks.margin = ggplot2::unit(0, "null"),
           panel.grid = ggplot2::element_blank(),
           panel.margin = ggplot2::unit(c(0, 0, 0, 0), "null"),
           legend.margin = ggplot2::unit(0, "null"),
           legend.position = "bottom",
           legend.key.width = ggplot2::unit(2, "cm"),
           legend.text = ggplot2::element_text(size = 10),
           legend.title = ggplot2::element_text(size = 10),
           panel.border = ggplot2::element_rect(color = NA, fill = NA),
           strip.background = ggplot2::element_rect(fill = "#333333"),
           strip.text = ggplot2::element_text(color = "white", size = 12)) +
         ggplot2::facet_wrap(~ season) +
         ggplot2::scale_fill_viridis(name = "Sightings")
  }
  # return result
  p
}

