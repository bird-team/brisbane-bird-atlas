#' Format audio data
#'
#' Format audio data for subsequent processing by the atlas.
#'
#' @param x \code{data.frame} data.
#'
#' @param scientific_column_name \code{character} name of column with the
#'   species' scientific names in \code{character} format.
#'
#' @param track_url_column_name \code{character} name of column with the
#'   file path (or url) of the audio files.
#'
#' @param track_title_column_name \code{character} name of column with the
#'   title of the audio files.
#'
#' @param author_name_column_name \code{character} name of column with the
#'   author of the audio files.
#'
#' @param author_url_column_name \code{character} name of column with the
#'   url for the track author
#'
#' @param copyright_image_column_name \code{character} name of column with the
#'   image for the copyright details
#'
#' @param copyright_url_column_name \code{character} name of column with the
#'   url for the copyright details
#'
#' @return \code{data.frame} with formatted data.
format_audio_data <- function(x, scientific_column_name, track_url_column_name,
                              track_title_column_name, author_name_column_name,
                              author_url_column_name,
                              copyright_image_column_name,
                              copyright_url_column_name) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"),
                          nrow(x) > 0,
                          assertthat::is.string(scientific_column_name),
                          assertthat::has_name(x, scientific_column_name),
                          is.character(x[[scientific_column_name]]),
                          assertthat::is.string(track_url_column_name),
                          assertthat::has_name(x, track_url_column_name),
                          is.character(x[[track_url_column_name]]),
                          assertthat::is.string(track_title_column_name),
                          assertthat::has_name(x, track_title_column_name),
                          is.character(x[[track_title_column_name]]),
                          assertthat::is.string(author_name_column_name),
                          assertthat::has_name(x, author_name_column_name),
                          is.character(x[[author_name_column_name]]),
                          assertthat::is.string(author_url_column_name),
                          assertthat::has_name(x, author_url_column_name),
                          is.character(x[[author_url_column_name]]),
                          assertthat::is.string(copyright_image_column_name),
                          assertthat::has_name(x, copyright_image_column_name),
                          is.character(x[[copyright_image_column_name]]),
                          assertthat::is.string(copyright_url_column_name),
                          assertthat::has_name(x, copyright_url_column_name),
                          is.character(x[[copyright_url_column_name]]))
  # rename columns
  data.table::setnames(x,
                       c(scientific_column_name, track_url_column_name,
                         track_title_column_name, author_name_column_name,
                         author_url_column_name, copyright_image_column_name,
                         copyright_url_column_name),
                       c("species_scientific_name", "track_url",
                         "track_title", "author_name", "author_url",
                         "copyright_image", "copyright_url"))

  # remove rows with missing values
  x <- x[!is.na(x$species_scientific_name), , drop = FALSE]

  # return result
  x
}
