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
  x$month <- format(record_posix_dates, "%b")
  x$year <- format(record_posix_dates, "%Y")

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
