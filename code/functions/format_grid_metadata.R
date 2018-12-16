#' Format grid metadata
#'
#' Validate that the grid meta-data is formatted correctly, and matches
#' the spatial grid data.
#'
#' @param x \code{\link[sf]{sf}} object containing metadata for grid cells.
#'
#' @param grid_data \code{\link[sf]{sf}} object containing the grid cells
#'   represented as polygons.
#'
#' @param id_column_name \code{character} name of column in the argument to
#'   \code{x} with unique grid cell identifier. This should match the
#'   \code{"id"} column in the argument to \code{"grid_data"}.
#'
#' @param name_column_name \code{character} name of the column in the argument
#'   to \code{x} with the name of each grid cell.
#'
#' @param checklist_target_column_name \code{character} name of the column in
#'   the argument to \code{x} with the checklist target for each grid cell.
#'
#' @param minute_target_column_name \code{character} name of the column in the
#'   argument to \code{x} with the minute target for each grid cell.
#'
#' @param km_target_column_name \code{character} name of the column in the
#'   argument to \code{x} with the km target for each grid cell.
#'
#' @param description_column_name \code{character} name of the column in the
#'   argument to \code{x} with the description of each grid cell.
#'
#' @param map_type_column_name \code{character} name of the column in the
#'  argument to \code{x} which the map type.
#'
#' @return \code{data.frame} object.
format_grid_metadata <- function(x, grid_data, id_column_name,
                                 name_column_name,
                                 checklist_target_column_name,
                                 minute_target_column_name,
                                 km_target_column_name,
                                 description_column_name,
                                 map_type_column_name) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "data.frame"),
    inherits(grid_data, "sf"),
    assertthat::has_name(grid_data, "id"),
    is.numeric(grid_data$id),
    assertthat::noNA(grid_data$id),
    assertthat::is.string(id_column_name),
    assertthat::is.string(name_column_name),
    assertthat::is.string(checklist_target_column_name),
    assertthat::is.string(minute_target_column_name),
    assertthat::is.string(km_target_column_name),
    assertthat::is.string(description_column_name),
    assertthat::is.string(map_type_column_name),
    assertthat::has_name(x, id_column_name),
    assertthat::has_name(x, name_column_name),
    assertthat::has_name(x, checklist_target_column_name),
    assertthat::has_name(x, minute_target_column_name),
    assertthat::has_name(x, km_target_column_name),
    assertthat::has_name(x, description_column_name),
    assertthat::has_name(x, map_type_column_name),
    assertthat::noNA(x[[id_column_name]]),
    assertthat::noNA(x[[name_column_name]]),
    assertthat::noNA(x[[checklist_target_column_name]]),
    assertthat::noNA(x[[checklist_target_column_name]]),
    assertthat::noNA(x[[minute_target_column_name]]),
    assertthat::noNA(x[[km_target_column_name]]),
    assertthat::noNA(x[[description_column_name]]),
    assertthat::noNA(x[[map_type_column_name]]),
    is.numeric(x[[id_column_name]]),
    is.character(x[[name_column_name]]),
    is.numeric(x[[checklist_target_column_name]]),
    is.numeric(x[[minute_target_column_name]]),
    is.numeric(x[[km_target_column_name]]),
    is.character(x[[description_column_name]]),
    is.character(x[[map_type_column_name]]),
    nrow(x) == nrow(grid_data),
    all(x[[id_column_name]] %in% grid_data$id),
    anyDuplicated(x[[id_column_name]]) == 0)

  # rename columns
  data.table::setnames(x,
                       c(id_column_name,
                         name_column_name,
                         checklist_target_column_name,
                         minute_target_column_name,
                         km_target_column_name,
                         description_column_name,
                         map_type_column_name),
                       c("id", "name", "checklist_target", "minute_target",
                         "km_target", "description", "map_type"))

  # return result
  x
}
