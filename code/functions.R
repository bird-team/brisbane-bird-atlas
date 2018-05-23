#' Format eBird record data
#'
#' Format eBird observation records for subsequent processing by the atlas.
#'
#' @param x \code{data.frame} eBird record data.
#'
#' @param scientific_column_name \code{character} name of column with the
#'   species' scientific names in \code{character} format.
#'
#' @param date_column_name \code{character} name of column with the
#'   observation dates for each record in \code{character} format..
#'
#' @param date_column_format \code{character} format of the dates in the
#'   argument to \code{x}.

#' @param longitude_column_name \code{character} name of column with the
#'   \code{numeric} longitude where each record was observed.
#'
#' @param latitude_column_name \code{character} name of column with the
#'   \code{numeric} latitude where each record was observed.
#'
#' @param start_date \code{character} earliest possible date for records.
#'   Records that correspond to observations prior to this date will be
#'   omitted. The argument to \code{start_date} must be in the
#'   \code{"%d/%m/%Y"} format (e.g. \code{"30/01/1990"}).
#'
#' @param study_area \code{sf} object delineating the extent of the study area.
#'   Records that do not overlap with the study area will be omitted.
#'
#' @return \code{\link[sf]{sf}} with the observation records and meta-data.
format_ebird_records <- function(x, scientific_column_name, date_column_name,
                                 date_column_format, longitude_column_name,
                                 latitude_column_name, start_date, study_area) {
   # assert that arguments are valid
   assertthat::assert_that(inherits(x, "data.frame"),
                           nrow(x) > 0,
                           assertthat::is.string(scientific_column_name),
                           assertthat::has_name(x, scientific_column_name),
                           is.character(x[[scientific_column_name]]),
                           assertthat::is.string(date_column_name),
                           assertthat::has_name(x, date_column_name),
                           is.character(x[[date_column_name]]),
                           assertthat::is.string(date_column_format),
                           assertthat::is.string(longitude_column_name),
                           assertthat::has_name(x, longitude_column_name),
                           is.numeric(x[[longitude_column_name]]),
                           assertthat::is.string(latitude_column_name),
                           assertthat::has_name(x, latitude_column_name),
                           is.numeric(x[[latitude_column_name]]),
                           assertthat::is.string(start_date),
                           inherits(study_area, "sf"))
  # parse record start date
  start_date <- as.POSIXct(strptime(start_date, "%d/%m/%Y"))
  assertthat::assert_that(all(!is.na(start_date)),
    msg = "argument to start_date must be formatted as %d/%m/%Y")

  # rename columns in the table
  data.table::setnames(x,
                       c(scientific_column_name, date_column_name,
                         longitude_column_name, latitude_column_name),
                      c("species_scientific_name", "date", "longitude",
                        "latitude"))

  # create formatted date data
  record_original_na_dates <- sum(is.na(x$date))
  record_posix_dates <- as.POSIXct(strptime(x$date, date_column_format))
  x$date <- format(record_posix_dates, "%d/%m/%Y")

  # create column with season data
  month <- format(record_posix_dates, "%m")
  month[month == 12] <- 0
  season <- character(length(month))
  season[month <= 2] <- "summer"
  season[month >= 3 & month <= 5] <- "autumn"
  season[month >= 6 & month <= 8] <- "winter"
  season[month >= 9] <- "spring"
  x$season <- season

  # check date conversions worked
  assertthat::assert_that(sum(is.na(x$date)) == record_original_na_dates,
                          msg = "error formatting dates in recorded data")

  # subset records to only those after the start date
  x <- x[na.omit(record_posix_dates >= start_date), , drop = FALSE]

  # remove records not identified to species level
  x <- x[!grepl("sp.", x$species_scientific_name, fixed = TRUE), , drop = FALSE]

  # convert data.frame to sf object
  x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326,
                agr = "constant")

  # transform to specified crs
  x <- sf::st_transform(x, sf::st_crs(study_area_data))

  # remove records not inside study area
  x <- x[as.matrix(sf::st_intersects(x, study_area_data))[, 1], ]

  # return result
  x
}

#' Format eBird taxonomy data
#'
#' Format eBird taxonomy data for subsequent processing by the atlas.
#'
#' @param x \code{data.frame} eBird record data.
#'
#' @param scientific_column_name \code{character} name of column with the
#'   species' scientific names in \code{character} format.
#'
#' @param common_column_name \code{character} name of column with the
#'   species' common names in \code{character} format.
#'
#' @param family_column_name \code{character} name of column with the
#'   species' family names in \code{character} format.
#'
#' @param order_column_name \code{character} name of column with the
#'   species' order names in \code{character} format.
#'
#' @param sort_column_name \code{character} name of column with information
#'   for ordering the species in the atlas.
#'
#' @return \code{data.frame} with the taxonomy data.
format_ebird_taxonomy <- function(x, scientific_column_name,
                                  common_column_name, family_column_name,
                                  order_column_name, sort_column_name) {
   # assert that arguments are valid
   assertthat::assert_that(inherits(x, "data.frame"),
                           nrow(x) > 0,
                           assertthat::is.string(scientific_column_name),
                           assertthat::has_name(x, scientific_column_name),
                           is.character(x[[scientific_column_name]]),
                           assertthat::is.string(common_column_name),
                           assertthat::has_name(x, common_column_name),
                           is.character(x[[common_column_name]]),
                           assertthat::is.string(family_column_name),
                           assertthat::has_name(x, family_column_name),
                           is.character(x[[family_column_name]]),
                           assertthat::is.string(order_column_name),
                           assertthat::has_name(x, order_column_name),
                           is.character(x[[order_column_name]]),
                           assertthat::is.string(sort_column_name),
                           assertthat::has_name(x, sort_column_name))
  # set column names
  data.table::setnames(x,
                       c(scientific_column_name, common_column_name,
                         family_column_name, order_column_name,
                         sort_column_name),
                       c("species_scientific_name", "species_common_name",
                         "family", "order_scientific_name",
                         "species_sorting_key"))

  # remove duplicated
  x <- x[!duplicated(x$species_scientific_name), ]

  # remove taxa not identified to sp. level
  x <- x[!grepl("sp.", x$species_scientific_name, fixed = TRUE), , drop = FALSE]

  # add in family common name
  x$family_scientific_name <- vapply(strsplit(x$family, " ", fixed = TRUE),
                                     `[[`, character(1), 1)
  x$family_common_name <- vapply(strsplit(x$family, "(", fixed = TRUE),
                                 `[[`, character(1), 2)
  x$family_common_name <- gsub(")", "", x$family_common_name, fixed = TRUE)

  # sort data by key
  x <- x[order(x$species_sorting_key, x$order_scientific_name,
               x$family_scientific_name, x$species_scientific_name), ,
               drop = FALSE]

  # select relevant columns
  x <- x[, c("species_scientific_name", "species_common_name",
              "family_scientific_name", "family_common_name",
               "order_scientific_name", "species_sorting_key"), drop = FALSE]

  # return result
  x
}

#' Format species name as title
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name and common name
#'   of the species. The argument to \code{data} must have the columns
#'   \code{"species_common_name"} and \code{"species_scientific_name"}.
#'
#' @return \code{character} markdown formatted title.
species_title <- function(x, data) {
  paste0(data$species_common_name[which(data$species_scientific_name == x)[1]],
         "(_", x, "_)")
}

#' Format family name as title
#'
#' @param x \code{character} scientific name of the family.
#'
#' @param data \code{data.frame} containing the scientific name and common name
#'   of the species. The argument to \code{data} must have the columns
#'   \code{"family_common_name"} and \code{"family_scientific_name"}.
#'
#' @return \code{character} markdown formatted title.
family_title <- function(x, data) {
  paste0(data$family_common_name[which(data$family_scientific_name == x)[1]],
         "(_", x, "_)")
}

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
