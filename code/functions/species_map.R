#' Plot species map
#'
#' Create maps showing spatio-temporal distribution of species records.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"} and \code{"season"}.
#'
#' @param grid_data \code{\link[raster]{RasterLayer} object containing the grid
#'   cells for displaying data on the map.
#'
#' @param land_data \code{sf} object showing the land masses. This
#'   is not used if the argument to \code{interactive} is \code{FALSE}
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_map <- function(x, record_data, grid_data, land_data) {
  # initialization
  ## subset record data
  record_data <- record_data[record_data$species_scientific_name == x, ,
                             drop = FALSE]
  ## create raster stack to store frequency data
  layer_names <- c("summer", "autumn", "winter", "spring")
  grid_data <- raster::stack(grid_data, grid_data, grid_data, grid_data)
  names(grid_data) <- layer_names
  ## calculate frequency of records in grid cells
  cells <- raster::extract(grid_data[[1]], as(record_data[, "year"], "Spatial"),
                           cellnumbers = TRUE)[, 1]
  for (l in layer_names[-1]) {
    curr_tbl <- as.data.frame(table(cells[record_data$season == l]))
    if (nrow(curr_tbl) > 0) {
       curr_tbl[[1]] <- as.integer(as.character(curr_tbl[[1]]))
       grid_data[[l]][curr_tbl[[1]]] <- curr_tbl[[2]]
    }
  }
  ## create group names
  group_names <- c("Summer", "Autumn", "Winter", "Spring")
  names(grid_data) <- group_names
  # main processing
  ## set boundary box data
  bb <- sf::st_bbox(grid_data)
  ## format land data
  land_data$name <- NULL
  ## format data for plotting
  plot_data <- as.data.frame(grid_data, xy = TRUE, na.rm = TRUE)
  plot_data$cell <- seq_len(nrow(plot_data))
  plot_data <- tidyr::gather(plot_data, name, value, -x, -y, -cell)
  plot_data$name <- factor(plot_data$name, levels = group_names)
  ## create plot
  p <- ggplot2::ggplot() +
       ggplot2::geom_sf(data = land_data, color = "#333333",
                        fill = "#333333") +
       ggplot2::geom_tile(data = plot_data,
                          ggplot2::aes(x = x, y = y, fill = value)) +
       ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
       ggplot2::facet_wrap(~ name) +
       viridis::scale_fill_viridis(name = "Sightings") +
       ggplot2::theme(
         #axis.ticks = ggplot2::element_blank(),
         #axis.text = ggplot2::element_blank(),
         axis.title = ggplot2::element_blank(),
         #axis.line = ggplot2::element_blank(),
         #axis.ticks.length = ggplot2::unit(0, "null"),
         #axis.ticks.margin = ggplot2::unit(0, "null"),
         panel.background = ggplot2::element_rect(color = "black", fill = NA),
         panel.border = ggplot2::element_rect(color = NA, fill = NA),
         panel.grid = ggplot2::element_blank(),
         panel.grid.major = element_line(colour = "transparent"),
         #legend.margin = ggplot2::unit(0, "null"),
         legend.key.height = ggplot2::unit(2, "cm"),
         legend.text = ggplot2::element_text(size = 10),
         legend.title = ggplot2::element_text(size = 10),
         strip.background = ggplot2::element_blank(),
         strip.text = ggplot2::element_text(color = "black", size = 12))
  # return result
  p
}
