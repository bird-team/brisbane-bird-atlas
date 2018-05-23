#' Format species data
#'
#' Format species data for subsequent processing by the atlas.
#'
#' @param x \code{data.frame} data.
#'
#' @param scientific_column_name \code{character} name of column with the
#'   species' scientific names in \code{character} format.
#'
#' @param key_column_name \code{numeric} name of column with the values with
#'   which to sort species in the atlas.
#'
#' @return \code{data.frame} with formatted data.
format_species_data <- function(x, scientific_column_name, key_column_name) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"),
                          nrow(x) > 0,
                          assertthat::is.string(scientific_column_name),
                          assertthat::has_name(x, scientific_column_name),
                          is.character(x[[scientific_column_name]]),
                          !anyDuplicated(x[[scientific_column_name]]),
                          assertthat::is.string(key_column_name),
                          assertthat::has_name(x, key_column_name),
                          is.numeric(x[[key_column_name]]))
  # rename columns
  data.table::setnames(x,
                       c(scientific_column_name, key_column_name),
                       c("species_scientific_name", "species_key"))

  # select relevant columns
  x <- x[, c("species_scientific_name", "species_key"), drop = FALSE]

  # sort data
  x <- x[order(x$species_scientific_name, x$species_key), , drop = FALSE]

  # return result
  x
}
