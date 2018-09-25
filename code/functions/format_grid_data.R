#' Format grid data
#'
#' Validate that the grid data is formatted correctly, and add a \code{"type"}
#' field indicating which cells are \code{"land"} or \code{"marine"}.
#'
#' @param x \code{\link[sf]{sf}} object containing the grid cells represented
#'   as polygons.
#'
#' @param study_area_data \code{\link[sf]{sf}} object defining the study area.
#'   Note that this object must a \code{"name"} field indicating which
#'   areas are \code{"land"} or \code{"marine"}.
#'
#' @param grid_resolution \code{numeric} resolution that the grid cells should
#'   exhibit.
#'
#' @return \code{\link[sf]{sf}} object.
format_grid_data <- function(x, study_area_data, grid_resolution) {
  # validate arguments
  assertthat::assert_that(inherits(x, "sf"),
                          assertthat::has_name(x, "id"),
                          assertthat::has_name(x, "name"),
                          is.numeric(x$id),
                          assertthat::noNA(x$id),
                          is.character(x$name),
                          assertthat::noNA(x$name),
                          anyDuplicated(x$id) == 0,
                          raster::compareCRS(
                            sp::CRS(sf::st_crs(study_area_data)[[2]]),
                            sp::CRS(sf::st_crs(study_area_data)[[2]])))
  assertthat::assert_that(inherits(study_area_data, "sf"),
                          assertthat::has_name(study_area_data, "name"),
                          is.character(study_area_data$name),
                          assertthat::noNA(study_area_data$name),
                          all(study_area_data$name %in% c("land", "marine")))
  assertthat::assert_that(assertthat::is.scalar(grid_resolution),
                          assertthat::noNA(grid_resolution),
                          all(abs((grid_resolution ^ 2) -
                                  as.numeric(sf::st_area(x))) <
                              1e-10))
  # coerce crs to be exactly equal
  suppressWarnings({sf::st_crs(x) <- sf::st_crs(study_area_data)})

  # assign type column
  ## assign marine grid cells
  x$marine <- x %>%
              sf::st_intersects(study_area_data %>%
                                filter(name == "marine")) %>%
              as.matrix() %>%
              c()

  ## assign terrestrial grid cells
  x$land <- x %>%
            sf::st_intersects(study_area_data %>%
                              filter(name == "land")) %>%
            as.matrix() %>%
            c()

  ## assign type field
  x$type <- NA_character_
  x$type[x$marine] <- "marine"
  x$type[x$land] <- "land"

  ## select relevant columns
  x <- x %>% select(id, type, name)

  # return result
  x
}
