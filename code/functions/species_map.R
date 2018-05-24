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
#' @param land_data \code{sf} object showing the land masses.
#'
#' @param study_area_data \code{sf} object showing the study area.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_map <- function(x, record_data, grid_data, land_data,
                        study_area_data) {
  # Initialization
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## create check list data with all check lists
  chk_data <- record_data[record_data$is_checklist,
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
  layer_names <- c("summer", "autumn", "winter", "spring")
  grid_data <- raster::stack(grid_data, grid_data, grid_data, grid_data)
  names(grid_data) <- layer_names
  ## calculate frequency of checklists in grid cells
  spp_cells <- raster::extract(grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(grid_data[[1]],
                               as(chk_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  for (l in layer_names) {
    # extract grid cells
    spp_tbl <- as.data.frame(table(spp_cells[spp_data$season == l]))
    chk_tbl <- as.data.frame(table(chk_cells[chk_data$season == l]))
    # skip if no checklists at all in this season for this species
    if (nrow(chk_tbl) > 0) {
      chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
      if (nrow(spp_tbl) == 0) {
        # assign zeros to calls with checklists for other species
        grid_data[[l]][chk_tbl[[1]]] <- 0
      } else {
        # assign total number of check lists to grid cells
        chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
        grid_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
        # calculate reporting rate for cells with checklists
        spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
        grid_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
                                        grid_data[[l]][spp_tbl[[1]]]
        # assign zeros to cells with checklists where thiss species wasn't
        # detected
        grid_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- 0
      }
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
  plot_data <- raster::as.data.frame(grid_data, xy = TRUE, na.rm = TRUE)
  plot_data$cell <- seq_len(nrow(plot_data))
  plot_data <- tidyr::gather(plot_data, name, value, -x, -y, -cell)
  plot_data$name <- factor(plot_data$name, levels = group_names)
  ## create plot
  p <- ggplot2::ggplot() +
       ggplot2::geom_sf(data = land_data, color = "grey90",
                        fill = "grey90") +
       ggplot2::geom_sf(data = study_area_data, color = "grey70",
                        fill = "grey70") +
       ggplot2::geom_tile(data = plot_data,
                          ggplot2::aes(x = x, y = y, fill = value)) +
       ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
       ggplot2::facet_wrap(~ name) +
       viridis::scale_fill_viridis(name = "Reporting rate",
                                   labels = scales::percent,
                                   limits = c(0, 1),
                                   option = "C") +
       ggplot2::theme(
         axis.ticks = ggplot2::element_blank(),
         axis.text.y = ggplot2::element_blank(),
         axis.text.x = ggplot2::element_blank(),
         axis.title = ggplot2::element_blank(),
         axis.line = ggplot2::element_blank(),
         axis.ticks.length = ggplot2::unit(0, "null"),
         axis.ticks.margin = ggplot2::unit(0, "null"),
         panel.background = ggplot2::element_rect(color = "black", fill = NA),
         panel.border = ggplot2::element_rect(color = "black", fill = NA),
         panel.grid = ggplot2::element_blank(),
         panel.grid.major = ggplot2::element_line(colour = "transparent"),
         #legend.margin = ggplot2::unit(0, "null"),
         legend.key.height = ggplot2::unit(1.0, "cm"),
         legend.text = ggplot2::element_text(size = 10),
         legend.title = ggplot2::element_text(size = 10),
         strip.background = ggplot2::element_blank(),
         strip.text = ggplot2::element_text(color = "black", size = 12))
  # return result
  p
}
