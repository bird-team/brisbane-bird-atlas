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
