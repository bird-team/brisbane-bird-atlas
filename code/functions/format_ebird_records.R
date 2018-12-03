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
#'
#' @param longitude_column_name \code{character} name of column with the
#'   \code{numeric} longitude where each record was observed.
#'
#' @param latitude_column_name \code{character} name of column with the
#'   \code{numeric} latitude where each record was observed.
#'
#' @param event_column_name \code{character} name of column with the
#'   unique identifier for each sampling event.
#'
#' @param locality_column_name \code{character} name of column with the
#'   unique identifier for each locality in the data set.
#'
#' @param locality_name_column_name \code{character} name of column with the
#'   name for each locality in the data set.
#'
#' @param locality_type_column_name \code{character} name of column with the
#'   type for each locality in the data set.
#'
#' @param protocol_column_name \code{character} name of column with the
#'   name of the sampling methodology.
#'
#' @param all_species_column_name \code{character} name of column indicating if
#'   all the species observed in a sampling event were reported.
#'
#' @param count_column_name \code{character} name of column with number
#'   of individuals observed during a sampling event.
#'
#' @param omit_protocol_names \code{character} vector with names of
#'   sampling protocols to omit from reporting rate calculations.
#'
#' @param breeding_column_name \code{character} name of column with breeding
#'   activity information.
#'
#' @param breeding_activity_names \code{character} name of breeding activity
#'   values that count as confirmed breeding activity.
#'
#' @param distance_km_column_name \code{character} column name with the
#'   distance travelled per sampling event.
#'
#' @param duration_minutes_column_name \code{character} column name with the
#'   duration of the sampling event in minutes.
#'
#' @param observer_id_column_name \code{character} column name with the
#'   observer's unique identifier.
#'
#' @param study_area \code{sf} object delineating the extent of the study area.
#'   Records that do not overlap with the study area will be omitted.
#'
#' @return \code{\link[sf]{sf}} with the observation records and meta-data.
format_ebird_records <- function(x, scientific_column_name, date_column_name,
                                 date_column_format, longitude_column_name,
                                 latitude_column_name,
                                 event_column_name, locality_column_name,
                                 locality_name_column_name,
                                 locality_type_column_name,
                                 protocol_column_name, all_species_column_name,
                                 count_column_name, omit_protocol_names,
                                 breeding_column_name, breeding_activity_names,
                                 distance_km_column_name,
                                 duration_minutes_column_name,
                                 observer_id_column_name,
                                 study_area) {
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
                           assertthat::is.string(event_column_name),
                           assertthat::has_name(x, event_column_name),
                           is.character(x[[event_column_name]]),
                           assertthat::is.string(locality_column_name),
                           assertthat::has_name(x, locality_column_name),
                           is.character(x[[locality_column_name]]),
                           assertthat::is.string(locality_name_column_name),
                           assertthat::has_name(x, locality_name_column_name),
                           is.character(x[[locality_name_column_name]]),
                           assertthat::is.string(locality_type_column_name),
                           assertthat::has_name(x, locality_type_column_name),
                           is.character(x[[locality_type_column_name]]),
                           assertthat::is.string(protocol_column_name),
                           assertthat::has_name(x, protocol_column_name),
                           is.character(x[[protocol_column_name]]),
                           assertthat::is.string(all_species_column_name),
                           assertthat::has_name(x, all_species_column_name),
                           is.numeric(x[[all_species_column_name]]),
                           assertthat::is.string(count_column_name),
                           assertthat::has_name(x, count_column_name),
                           inherits(x[[count_column_name]],
                                    c("numeric", "character")),
                           assertthat::is.string(distance_km_column_name),
                           assertthat::has_name(x, distance_km_column_name),
                           is.numeric(x[[distance_km_column_name]]),
                           assertthat::is.string(duration_minutes_column_name),
                           assertthat::has_name(x,
                             duration_minutes_column_name),
                           is.numeric(x[[duration_minutes_column_name]]),
                           assertthat::is.string(observer_id_column_name),
                           assertthat::has_name(x, observer_id_column_name),
                           is.character(x[[observer_id_column_name]]),
                           is.character(omit_protocol_names),
                           assertthat::noNA(omit_protocol_names),
                           assertthat::is.string(breeding_column_name),
                           assertthat::has_name(x, breeding_column_name),
                           is.character(x[[breeding_column_name]]),
                           is.character(breeding_activity_names),
                           assertthat::noNA(breeding_activity_names),
                           inherits(study_area_data, "sf"))

  # rename columns in the table
  data.table::setnames(x,
                       c(scientific_column_name, date_column_name,
                         longitude_column_name, latitude_column_name,
                         event_column_name, count_column_name,
                         locality_column_name, locality_name_column_name,
                         locality_type_column_name, protocol_column_name,
                         distance_km_column_name, duration_minutes_column_name,
                         observer_id_column_name),
                      c("species_scientific_name", "date", "longitude",
                        "latitude", "event", "count", "locality", "locality_name", "locality_type",
                        "protocol", "distance_km", "duration_minutes",
                        "observer_id"))

  # coerce count column to numeric
  x$count <- as.numeric(x$count)

  # create is_checklist column indicating if data corresponds to a "true"
  # checklist for calculating reporting rates
  x$is_checklist <- (x[[all_species_column_name]] == 1) &
                    (!x$protocol %in% omit_protocol_names)

  # create is_breeding column indicating if data corresponds to an event
  # with confirmed breeding activity
  x$is_breeding <- x[[breeding_column_name]] %in% breeding_activity_names

  # create formatted date data
  record_original_na_dates <- sum(is.na(x$date))
  record_posix_dates <- as.POSIXct(strptime(x$date, date_column_format))
  x$date <- format(record_posix_dates, "%d/%m/%Y")
  x$month <- format(record_posix_dates, "%b")
  x$year <- as.numeric(format(record_posix_dates, "%Y"))

  # create column with season data
  month <- as.integer(format(record_posix_dates, "%m"))
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

  # create column indicating if record in fully sampled year
  # (i.e. latest year with records in December)
  x$is_fully_sampled_year <- x$year <= max(x$year[x$month == "Dec"])

  # remove records not identified to species level
  x <- x[!grepl("sp.", x$species_scientific_name, fixed = TRUE), , drop = FALSE]

  # convert data.frame to sf object and preserve longitudes and latitudes
  lon <- x$longitude
  lat <- x$latitude
  x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326,
                    agr = "constant")
  x$longitude <- lon
  x$latitude <- lat

  # transform to specified crs
  x <- sf::st_transform(x, sf::st_crs(study_area))

  # create subset with unique localities
  l <- x[!duplicated(x$locality), "locality"]
  l <- l[as.matrix(sf::st_intersects(l, study_area))[, 1], ]

  # remove records not inside study area using localities
  x <- x[x$locality %in% l$locality, ]

  # return result
  x
}
