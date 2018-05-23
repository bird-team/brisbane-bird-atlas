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
                                  order_column_name) {
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
                           is.character(x[[order_column_name]]))
  # set column names
  data.table::setnames(x,
                       c(scientific_column_name, common_column_name,
                         family_column_name, order_column_name),
                       c("species_scientific_name", "species_common_name",
                         "family", "order_scientific_name"))

  # remove duplicated
  x <- x[!duplicated(x$species_scientific_name), , drop = FALSE]

  # remove taxa not identified to sp. level
  x <- x[!grepl("sp.", x$species_scientific_name, fixed = TRUE), , drop = FALSE]

  # add in family common name
  x$family_scientific_name <- vapply(strsplit(x$family, " ", fixed = TRUE),
                                     `[[`, character(1), 1)
  x$family_common_name <- vapply(strsplit(x$family, "(", fixed = TRUE),
                                 `[[`, character(1), 2)
  x$family_common_name <- gsub(")", "", x$family_common_name, fixed = TRUE)

  # select relevant columns
  x <- x[, c("species_scientific_name", "species_common_name",
             "family_scientific_name", "family_common_name",
             "order_scientific_name"), drop = FALSE]

  # return result
  x
}
