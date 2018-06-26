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
#'   \code{"species_scientific_name"}, \code{"season"}, \code{"is_checklist"},
#'   \code{"is_fully_sampled_year"}, \code{"year"} and \code{"maps"}.
#'
#' @param grid_data \code{\link[raster]{RasterLayer} object containing the grid
#'   cells for displaying data on the map.
#'
#' @param land_data \code{sf} object showing the land masses.
#'
#' @param study_area_data \code{sf} object showing the study area.
#'
#' @param minimum_required_checklists \code{numeric} number of checklists
#'   required for a grid cell to be adequately sampled for reporting rates.
#'
#' @param minimum_required_events \code{numeric} number of records required
#'   for a grid cell to be adequately sampled for range estimates.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_map <- function(x, species_data, record_data, grid_data, land_data,
                        study_area_data, minimum_required_checklists,
                        minimum_required_events) {
  # Initialization
  ## determine if grid should be land, marine, or both
  spp_index <- which(species_data$species_scientific_name == x)
  spp_type <- species_data$distribution[spp_index]
  ## set up grid
  if (spp_type == "land") {
    tmp_data <- grid_data
    grid_data[] <- 0
    grid_data[tmp_data == 1] <- NA_real_
    grid_data <- raster::trim(grid_data, values = 0)
  } else if (spp_type == "marine") {
    tmp_data <- grid_data
    grid_data[] <- 0
    grid_data[tmp_data == 2] <- NA_real_
    grid_data <- raster::trim(grid_data, values = 0)
  } else {
    tmp_data <- grid_data
    grid_data[is.na(tmp_data)] <- 0
    grid_data[!is.na(tmp_data)] <- NA_real_
    grid_data <- raster::trim(grid_data, values = 0)
  }
  ## set up study area data
  if (spp_type == "land") {
    study_area_data <- study_area_data %>%
                       filter(name == "land")
  } else if (spp_type == "marine") {
    study_area_data <- study_area_data %>%
                       filter(name == "marine")
  }
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## determine which maps to create
  map_numbers <- species_data$maps[spp_index]
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
  ## determine starting years for records and checklists
  checklists_starting_year <- species_data$checklists_starting_year[
                                    spp_index]
  records_starting_year <- species_data$records_starting_year[spp_index]
  ## create check list data with all check lists
  chk_data <- record_data[record_data$is_checklist &
                          record_data$is_fully_sampled_year &
                          record_data$year >= checklists_starting_year,
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
  spp_cells <- raster::extract(rate_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(rate_data[[1]],
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
  record_subset_data <- record_data %>%
                        dplyr::filter(year >= records_starting_year)
  chk_cells2 <- raster::extract(grid_data[[1]],
    as(record_subset_data[!duplicated(record_subset_data$event), "season"],
       "Spatial"),
    cellnumbers = TRUE)[, 1]
  spp_cells2 <- raster::extract(grid_data[[1]],
    as(record_subset_data[record_subset_data$species_scientific_name == x,
                          "season"],
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
  group_names <- c("All year", "Summer", "Autumn", "Winter", "Spring",
                   "Detection")
  complete_data <- raster::addLayer(rate_data, detection_data)
  names(complete_data) <- group_names
  ## subset data to specified map numbers
  complete_data <- complete_data[[map_numbers]]
  # main processing
  ## set boundary box data
  bb <- sf::st_bbox(complete_data)
  ## format land data
  land_data$name <- NULL
  ## format data for plotting
  plot_data <- raster::as.data.frame(complete_data, xy = TRUE, na.rm = FALSE)
  plot_data$cell <- seq_len(nrow(plot_data))
  plot_data <- tidyr::gather(plot_data, name, value, -x, -y, -cell)
  plot_data$name <- gsub(".", " ", plot_data$name, fixed = TRUE)
  plot_data$name <- factor(plot_data$name, levels = group_names[map_numbers])
  plot_data <- plot_data[plot_data$value > 1e-100 | is.na(plot_data$value), ]
  ## create colors
  palette <- color_numeric_palette("viridis", c(1, 100), na.color = "grey70")
  ## create data for legend
  if (6 %in% map_numbers) {
    legend_data <- data.frame(x = 0, y = 0,
      label = factor(c("0% Rate", "Unknown", "Detected"),
                     levels = c("0% Rate", "Unknown", "Detected")))
    legend_list <- list(color = rep("black", 3),
                        fill = c("white", "grey70", "red"))
  } else {
    legend_data <- data.frame(x = 0, y = 0,
                              label = c("0% Rate", "Unknown"))
    legend_list <- list(color = c("black", "black"),
                        fill = c("white", "grey70"))
  }
  ## set legend box spacing
  if (length(map_numbers) < 4) {
    if ((6 %in% map_numbers) && any(seq_len(5) %in% map_numbers)) {
      key_spacing <- 1
      key_height <- 0.4
      legend_key_height <- ggplot2::unit(0.01, "line")
      legend_key_width <- legend_key_height
    } else if (!(6 %in% map_numbers) && any(seq_len(5) %in% map_numbers)) {
      key_spacing <- 1
      key_height <- 0.4
      legend_key_height <- NULL
      legend_key_width <- NULL
    } else if ((6 %in% map_numbers) && !any(seq_len(5) %in% map_numbers)) {
      key_spacing <- 1
      key_height <- 0.4
      legend_key_height <- NULL
      legend_key_width <- NULL
    } else {
      key_spacing <- 1
      key_height <- 0.4
      legend_key_height <- NULL
      legend_key_width <- NULL
    }
  } else {
    key_spacing <- 1
    key_height <- 1
    legend_key_height <- NULL
    legend_key_width <- NULL
  }
  ## create plot
  p <- ggplot2::ggplot() +
       ggplot2::geom_point(data = legend_data,
                          ggplot2::aes(x = x, y = y, color = label),
                          shape = 22, size = 8) +
       ggplot2::geom_sf(data = land_data, color = "grey85",
                        fill = "grey90") +
       ggplot2::geom_sf(data = study_area_data, color = NA,
                        fill = "white")
  ## add all year or seasonal tiles if required
  if (any(seq_len(5) %in% map_numbers)) {
    p <- p +
         ggplot2::geom_tile(data = plot_data[plot_data$name != "Detection", ],
                            na.rm = FALSE,
                            ggplot2::aes(x = x, y = y, fill = value)) +
         ggplot2::scale_fill_gradientn(
           colors = palette(seq(1, 100)),
           na.value = "grey70", name = "Rate (%)", limits = c(0, NA_real_),
           labels = function(x) {
             z <- which(abs(x) < 1e-30)
               if (length(z) == 0) return(as.character(x))
               x <- as.character(x)
               x[z] <- paste0(">0")
               x
             })
  }
  ## add detection tiles if required
  if (6 %in% map_numbers) {
    p <- p +
         ggplot2::geom_tile(
           data = plot_data[which(plot_data$name == "Detection" &
                                  plot_data$value > 0.5), ], na.rm = FALSE,
           ggplot2::aes(x = x, y = y), fill = "red")
    p <- p +
         ggplot2::geom_tile(
           data = plot_data[which(plot_data$name == "Detection" &
                            is.na(plot_data$value)), ], na.rm = FALSE,
           ggplot2::aes(x = x, y = y), fill = "grey70")
  }
  ## style maps
  p <- p +
       ggplot2::geom_sf(data = study_area_data, color = "black",
                        fill = NA) +
       ggplot2::coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
       ggplot2::facet_wrap(~ name) +
       ggplot2::theme(
         axis.ticks = ggplot2::element_blank(),
         axis.text.y = ggplot2::element_blank(),
         axis.text.x = ggplot2::element_blank(),
         axis.title = ggplot2::element_blank(),
         axis.line = ggplot2::element_blank(),
         axis.ticks.length = ggplot2::unit(0, "null"),
         panel.background = ggplot2::element_rect(color = "black",
                                                  fill = "lightcyan"),
         panel.border = ggplot2::element_rect(color = "black", fill = NA),
         panel.grid = ggplot2::element_blank(),
         panel.grid.major = ggplot2::element_line(colour = "transparent"),
         legend.position = "right",
         legend.spacing.y = ggplot2::unit(key_spacing, "pt"),
         legend.key = ggplot2::element_blank(),
         legend.key.height = ggplot2::unit(key_height, "cm"),
         legend.text = ggplot2::element_text(size = 10),
         legend.title = ggplot2::element_text(size = 10, vjust = 1.5),
         strip.background = ggplot2::element_blank(),
         strip.text = ggplot2::element_text(color = "black", size = 12)) +
        ggplot2::guides(
          color = ggplot2::guide_legend(title = NULL, order = 98,
            override.aes = legend_list, keyheight = legend_key_height,
            keywidth = legend_key_width),
          fill = ggplot2::guide_colorbar(order = 97))
  # return result
  p
}
