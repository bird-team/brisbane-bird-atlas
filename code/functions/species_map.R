#' Plot species map
#'
#' Create maps showing spatio-temporal distribution of species records.
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
species_map <- function(x, species_data, record_data, grid_data, land_data,
                        study_area_data) {
  # Initialization
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## determine which graphs to create
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
  seasonal_map_numbers <- map_numbers[map_numbers %in% seq_len(4)]
  ## create observation data with all observations per species
  obs_data <- record_data[record_data$species_scientific_name == x,
                          c("species_scientific_name", "season", "event"),
                          drop = FALSE]
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
  if (length(seasonal_map_numbers) > 0) {
    layer_names <- c("summer", "autumn", "winter",
                     "spring")[seasonal_map_numbers]
    seasonal_grid_data <- raster::stack(grid_data, grid_data, grid_data,
                                        grid_data)[[seasonal_map_numbers]]
    names(seasonal_grid_data) <- layer_names
    if (raster::nlayers(seasonal_grid_data) == 1)
      seasonal_grid_data <- raster::stack(seasonal_grid_data)
    ## calculate frequency of checklists in grid cells for seasonal data
    spp_cells <- raster::extract(grid_data,
                                 as(spp_data[, "season"], "Spatial"),
                                 cellnumbers = TRUE)[, 1]
    chk_cells <- raster::extract(grid_data,
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
          seasonal_grid_data[[l]][chk_tbl[[1]]] <- 0
        } else {
          # assign total number of check lists to grid cells
          chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
          seasonal_grid_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
          # calculate reporting rate for cells with checklists
          spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
          seasonal_grid_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
            seasonal_grid_data[[l]][spp_tbl[[1]]]
          # assign zeros to cells with checklists where thiss species wasn't
          # detected
          seasonal_grid_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- 0
        }
      }
    }
    ## create group names
    group_names <- c("Summer", "Autumn", "Winter",
                     "Spring")[seasonal_map_numbers]
    names(seasonal_grid_data) <- group_names
  }
  ## calculate frequency of records in grid cells
  if (5 %in% map_numbers) {
    obs_cells <- raster::extract(grid_data,
                                 as(obs_data[, "season"], "Spatial"),
                                 cellnumbers = TRUE)[, 1]
    record_grid_data <- grid_data
    names(record_grid_data) <- "Records"
    obs_tbl <- as.data.frame(table(obs_cells))
    obs_tbl[[1]] <- as.integer(as.character(obs_tbl[[1]]))
    record_grid_data[obs_tbl[[1]]] <- obs_tbl[[2]]
  }
  # main processing
  ## set boundary box data
  bb <- sf::st_bbox(grid_data)
  ## format land data
  land_data$name <- NULL
  ## format data for plotting
  if (length(seasonal_map_numbers) > 0) {
    plot_data <- raster::as.data.frame(seasonal_grid_data, xy = TRUE,
                                       na.rm = TRUE)
    plot_data$cell <- seq_len(nrow(plot_data))
    plot_data <- tidyr::gather(plot_data, name, value, -x, -y, -cell)
    plot_data$name <- factor(plot_data$name, levels = group_names)
    plot_data$value <- plot_data$value * 100
    p1 <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = land_data, color = "grey90",
                           fill = "grey90") +
          ggplot2::geom_sf(data = study_area_data, color = "grey70",
                           fill = "grey70") +
          ggplot2::geom_tile(data = plot_data,
                             ggplot2::aes(x = x, y = y, fill = value)) +
          ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
          ggplot2::facet_wrap(~ name) +
          viridis::scale_fill_viridis(name = "Rate (%)",
                                      limits = c(0, 100),
                                      option = "C") +
          ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks.length = ggplot2::unit(0, "null"),
            panel.background = ggplot2::element_rect(color = "black",
                                                     fill =  NA),
            panel.border = ggplot2::element_rect(color = "black", fill = NA),
            panel.grid = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "transparent"),
            legend.position = "right",
            legend.key.height = ggplot2::unit(1.0, "cm"),
            legend.text = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_text(size = 10),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "black", size = 12))
  }
  ## create record plot
  if (5 %in% map_numbers) {
    plot_data <- raster::as.data.frame(record_grid_data, xy = TRUE,
                                       na.rm = TRUE)
    plot_data$cell <- seq_len(nrow(plot_data))
    plot_data <- tidyr::gather(plot_data, name, value, -x, -y, -cell)
    plot_data$name <- factor(plot_data$name, levels = "Records")
    p2 <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = land_data, color = "grey90",
                           fill = "grey90") +
          ggplot2::geom_sf(data = study_area_data, color = "grey70",
                           fill = "grey70") +
          ggplot2::geom_tile(data = plot_data,
                             ggplot2::aes(x = x, y = y, fill = value)) +
          ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
          viridis::scale_fill_viridis(name = "Records", option = "C") +
          ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks.length = ggplot2::unit(0, "null"),
            panel.background = ggplot2::element_rect(color = "black",
                                                     fill =  NA),
            panel.border = ggplot2::element_rect(color = "black", fill = NA),
            panel.grid = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "transparent"),
            legend.position = "right",
            legend.key.height = ggplot2::unit(1.0, "cm"),
            legend.text = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_text(size = 10),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "black", size = 12))
  }
  # convert panels to single graph
  if ((5 %in% map_numbers) && (length(seasonal_map_numbers) == 0)) {
    p <- p2
  } else if (max(map_numbers) < 5) {
    p <- p1
  } else {
    p <- p1 + p2
  }
  # return result
  p
}
