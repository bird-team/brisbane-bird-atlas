#' Grid map
#'
#' This function creates a grid map for a surveyor sampling sheet.
#'
#' @param x \code{integer} grid cell identifier.
#'
#' @param grid_data \code{\link[sf]{sf} object containing the grid
#'   cells for displaying data on the map.
#'
#' @param locations_data \code{\link[sf]{sf}} object that contains the
#'   sample locality data (e.g. eBird hotspots).
#'
#' @param study_area_data \code{\link[sf]{sf}} object that outlines the
#'   the study area.
#'
#' @param grid_resolution \code{numeric} resolution of the sampling grid.
#'
#' @param google_zoom_level \code{integer} zoom level for map background.
#'   Defaults to 15.
#'
#' @param google_map_type \code{character} type of map background.
#'  Available options are: \code{"terrain"}, \code{"satellite"},
#'  \code{"roadmap"}, \code{"hybrid"}. Defaults to \code{"satellite"}.
#'
#' @param grid_line_color \code{character} color of the sampling grid lines.
#'
#' @param grid_line_width \code{numeric} width of the sampling grid lines.
#'
#' @param study_area_line_color \code{character} color of the study area
#'   extent lines.
#'
#' @param study_area_line_width \code{numeric} width of the study area
#'   extent lines.
#'
#' @return \code{\link[ggplot2]{gg}} ggplot2 plot object.
grid_map <- function(x, grid_data, locations_data,
                     study_area_data,
                     grid_resolution,
                     google_zoom_level = 15,
                     google_map_type = "satellite",
                     grid_line_color = "#ffffff",
                     grid_line_width = 0.5,
                     study_area_line_color = "#cfcfcf",
                     study_area_line_width = 0.75) {
  # assert that arguments are valid
  assertthat::assert_that(assertthat::is.number(x),
                          inherits(grid_data, "sf"),
                          inherits(locations_data, "sf"),
                          inherits(study_area_data, "sf"),
                          assertthat::is.number(google_zoom_level),
                          assertthat::is.string(google_map_type),
                          assertthat::is.string(grid_line_color),
                          assertthat::is.number(grid_line_width),
                          assertthat::is.string(study_area_line_color),
                          assertthat::is.number(study_area_line_width))
  # find extent of grid_cell
  curr_extent <- grid_data %>%
                 filter(id == x) %>%
                 sf::st_transform(4326) %>%
                 as("Spatial") %>%
                 raster::extent()
  curr_xlim <- c(curr_extent@xmin, curr_extent@xmax)
  curr_xlim <- curr_xlim + (c(-1, 1) * 0.1 * abs(diff(curr_xlim)))
  curr_ylim <- c(curr_extent@ymin, curr_extent@ymax)
  curr_ylim <- curr_ylim + (c(-1, 1) * 0.1 * abs(diff(curr_ylim)))
  # create centroids
  grid_centroid_data <- suppressWarnings(sf::st_centroid(grid_data))
  # calculate centroid of grid cell
  curr_wgs1984_centroid <- grid_centroid_data %>%
                           filter(id == x) %>%
                           sf::st_transform(4326) %>%
                           as("Spatial") %>%
                           slot(name = "coords")
  # find neighboring grid cells
  neighboring_indices <- rgeos::gWithinDistance(
    grid_centroid_data %>%
      filter(id == x) %>%
      as("Spatial"),
    grid_centroid_data %>%
      as("Spatial"),
    dist = grid_resolution * 1.1, byid = TRUE)
  neighboring_indices <- which(c(neighboring_indices))
  # prepare polygons for plotting
  pl <- grid_data %>%
        `[`(neighboring_indices, ) %>%
        sf::st_transform(4326) %>%
        as("Spatial") %>%
        {suppressMessages(ggplot2::fortify(.))} %>%
        dplyr::rename(x = long, y = lat)
  std <- study_area_data %>%
         sf::st_transform(4326) %>%
         as("Spatial") %>%
         {suppressMessages(ggplot2::fortify(.))} %>%
         dplyr::rename(x = long, y = lat)
  # prepare text for plotting
  l <- locations_data %>%
       filter(c(as.matrix(sf::st_intersects(
         locations_data, grid_data[x, ]))))
  if (nrow(l) > 0)
    l <- l %>%
         sf::st_transform(4326) %>%
         as("Spatial") %>%
         as.data.frame() %>%
         dplyr::rename(x = coords.x1, y = coords.x2)
  # download background of grid cell
  bg <- suppressMessages({
    ggmap::get_googlemap(center = curr_wgs1984_centroid,
                         zoom = google_zoom_level,
                         maptype = google_map_type,
                         scale = 2,
                         messaging = FALSE,
                         urlonly = FALSE,
                         force = TRUE,
                         filename = tempfile(fileext = ".png"),
                         language = "en-EN",
                         color = "color",
                         size = c(640, 640),
                         key = Sys.getenv("GOOGLE_TOKEN"))
  })
  # create map
  p <- suppressWarnings({
    ggmap::ggmap(bg, extent = "normal", maprange = FALSE) +
      ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = group),
                            data = std, color = study_area_line_color,
                            fill = NA,
                            size = 0.75) +
      ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = id),
                            data = pl, color = grid_line_color, fill = NA,
                            size = grid_line_width) +
      ggplot2::coord_fixed(xlim = curr_xlim, ylim = curr_ylim) +
      ggmap::theme_nothing()
  })
  # add labels
  if (nrow(l) > 0) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y,
                                 shape = locality_type), data = l,
                                 color = "white") +
             ggrepel::geom_label_repel(ggplot2::aes(x = x, y = y,
                                                    label = locality_name),
                                       data = l, color = "white",
                                       size = 2.0, label.size = 0,
                                       label.padding = 0.1, max.iter = 10000,
                                       fill = scales::alpha("black", 0.4),
                                       seed = 500, force = 10) +
             ggplot2::scale_shape_manual(values = c("H" = 17, "P" = 19))
  }
  # return map
  p
}
