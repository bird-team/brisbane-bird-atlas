#' Format species data
#'
#' Format species data for subsequent processing by the atlas.
#'
#' @param x \code{data.frame} data.
#'
#' @param scientific_column_name \code{character} name of column with the
#'   species' scientific names in \code{character} format.
#'
#' @param key_column_name \code{character} name of column with the values with
#'   \code{numeric} values used to sort species in the atlas.
#'
#' @param iucn_column_name \code{character} name of column with the
#'   IUCN threat status for the species in \code{character} format.
#'
#' @param national_column_name \code{character} name of column with the
#'   national threat status for the species in \code{character} format.
#'
#' @param qld_column_name \code{character} name of column with the
#'   Queensland threat status for the species in \code{character} format.
#'
#' @param graphs_column_name \code{character} name of column indicating
#'   which graphs should be displayed for each species in \code{character}
#'   format. The data in this cell should be integers separated by underscores,
#'   for example \code{"4_1"} means that graph 4 and graph 1 should be
#'   plotted for a particular species.
#'
#' @param maps_column_name \code{character} name of column indicating
#'   which graphs should be displayed for each species in \code{character}
#'   format. The data in this cell should be integers separated by underscores,
#'   for example \code{"4_1"} means that graph 4 and graph 1 should be
#'   plotted for a particular species.
#'
#' @param checklist_year_column_name \code{character} name of column indicating
#'  the earliest checklists that should be used for making species graphs
#'  and maps.
#'
#' @param record_year_column_name \code{character} name of column indicating
#'  the earliest records that should be used for making species graphs
#'  and maps.
#'
#' @return \code{data.frame} with formatted data.
format_species_data <- function(x, scientific_column_name, common_column_name,
                                key_column_name, iucn_column_name,
                                national_column_name, qld_column_name,
                                graphs_column_name, maps_column_name,
                                checklist_year_column_name,
                                record_year_column_name) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"),
                          nrow(x) > 0,
                          assertthat::is.string(scientific_column_name),
                          assertthat::has_name(x, scientific_column_name),
                          is.character(x[[scientific_column_name]]),
                          !anyDuplicated(x[[scientific_column_name]]),
                          assertthat::is.string(common_column_name),
                          assertthat::has_name(x, common_column_name),
                          is.character(x[[common_column_name]]),
                          assertthat::is.string(key_column_name),
                          assertthat::has_name(x, key_column_name),
                          is.numeric(x[[key_column_name]]),
                          assertthat::is.string(iucn_column_name),
                          assertthat::has_name(x, iucn_column_name),
                          is.character(x[[iucn_column_name]]),
                          assertthat::is.string(national_column_name),
                          assertthat::has_name(x, national_column_name),
                          is.character(x[[national_column_name]]),
                          assertthat::is.string(qld_column_name),
                          assertthat::has_name(x, qld_column_name),
                          is.character(x[[qld_column_name]]),
                          assertthat::is.string(graphs_column_name),
                          assertthat::has_name(x, graphs_column_name),
                          is.character(x[[graphs_column_name]]),
                          assertthat::is.string(maps_column_name),
                          assertthat::has_name(x, maps_column_name),
                          is.character(x[[maps_column_name]]),
                          assertthat::is.string(checklist_year_column_name),
                          assertthat::has_name(x, checklist_year_column_name),
                          is.numeric(x[[checklist_year_column_name]]),
                          assertthat::is.string(record_year_column_name),
                          assertthat::has_name(x, record_year_column_name),
                          is.numeric(x[[record_year_column_name]]))
  # rename columns
  data.table::setnames(x,
                       c(scientific_column_name, common_column_name,
                         key_column_name, iucn_column_name,
                         national_column_name, qld_column_name,
                         graphs_column_name, maps_column_name,
                         checklist_year_column_name,
                         record_year_column_name),
                       c("species_scientific_name", "species_common_name",
                         "species_key", "iucn_threat_status",
                         "national_threat_status", "qld_threat_status",
                         "graphs", "maps", "checklists_starting_year",
                         "records_starting_year"))

  # remove rows with missing values
  x <- x[!is.na(x$species_scientific_name), , drop = FALSE]

  # select relevant columns
  x <- x[, c("species_scientific_name", "species_common_name", "species_key",
             "iucn_threat_status", "national_threat_status",
             "qld_threat_status", "graphs", "maps", "checklists_starting_year",
             "records_starting_year"),
         drop = FALSE]

  # sort data
  x <- x[order(x$species_key, x$species_common_name), , drop = FALSE]

  # return result
  x
}
