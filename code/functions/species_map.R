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
#' @param colors \code{character} color codes for making color ramps.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_map <- function(x, species_data, record_data, grid_data, land_data,
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
  ## store the names of columns with the desired plot types
  plot_names <- c("All year", "Summer", "Autumn", "Winter", "Spring",
                  "Detection")[map_numbers]
  ## prepare data for plotting
  plot_data <- grid_data %>%
               as("Spatial") %>%
               {suppressMessages(ggplot2::fortify(.))} %>%
               dplyr::rename(x = long, y = lat) %>%
               dplyr::select(x, y, id) %>%
               dplyr::left_join(as.data.frame(grid_data[, plot_names]) %>%
                                dplyr::select(-geometry) %>%
                                dplyr::mutate(
                                  id = as.character(seq_len(nrow(grid_data)))),
                                by = "id") %>%
               tidyr::gather(name, value, -x, -y, -id) %>%
               dplyr::filter(value > 1e-10 | is.na(value)) %>%
               dplyr::mutate(name = gsub(".", " ", name, fixed = TRUE)) %>%
               dplyr::mutate(name = factor(name, levels = plot_names))
  # main processing
  ## set boundary box data
  bb <- sf::st_bbox(study_area_data)
  ## create colors
  palette <- color_numeric_palette(colors, c(1, 100), na.color = "grey70")
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
                        fill = "grey90", size = 0.3) +
       ggplot2::geom_sf(data = study_area_data, color = NA,
                        fill = "white", size = 0.3)
  ## add all year or seasonal polygons if required
  if (any(seq_len(5) %in% map_numbers)) {
    p <- p +
         ggplot2::geom_polygon(data = dplyr::filter(plot_data,
                                                    name != "Detection"),
                               na.rm = FALSE,
                               ggplot2::aes(x = x, y = y, fill = value,
                                            group = id)) +
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
  ## add detection polygons if required
  if (6 %in% map_numbers) {
    p <- p +
         ggplot2::geom_polygon(
           data = dplyr::filter(plot_data, name == "Detection", value > 0.5),
           na.rm = FALSE,
           ggplot2::aes(x = x, y = y, group = id), fill = "red")
    p <- p +
         ggplot2::geom_polygon(
           data = dplyr::filter(plot_data, name == "Detection", is.na(value)),
           na.rm = FALSE,
           ggplot2::aes(x = x, y = y, group = id), fill = "grey70")
  }
  ## style maps
  p <- p +
       ggplot2::geom_sf(data = study_area_data, color = "grey50",
                        fill = NA, size = 0.3) +
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
