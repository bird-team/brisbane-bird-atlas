#' Render species audio player
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name of the
#'   species and associated audio file paths. The argument to \code{data} must
#'    have the columns \code{"species_scientific_name"} and \code{"audio_path"}.
#'
#' @return \code{character} html code.
render_species_audio <- function(x, data) {
  # subset data to species
  data <- data[data$species_scientific_name == x, , drop = FALSE]
  # remove symbols to avoid issues with player
  data$track_title <- gsub("---", ", ", data$track_title, fixed = TRUE)
  data$track_title <- gsub("--", ", ", data$track_title, fixed = TRUE)
  data$track_title <- gsub("-", ", ", data$track_title, fixed = TRUE)
  data$track_title <- gsub("\"", "", data$track_title, fixed = TRUE)
  data$track_title <- gsub("\'", "", data$track_title, fixed = TRUE)
  data$track_title <- gsub("\\n", "", data$track_title, fixed = TRUE)
  data$author_name <- gsub("---", ", ", data$author_name, fixed = TRUE)
  data$author_name <- gsub("--", ", ", data$author_name, fixed = TRUE)
  data$author_name <- gsub("-", ", ", data$author_name, fixed = TRUE)
  data$author_name <- gsub("\"", "", data$author_name, fixed = TRUE)
  data$author_name <- gsub("\'", "", data$author_name, fixed = TRUE)
  data$author_name <- gsub("\\n", "", data$author_name, fixed = TRUE)
  # make widget based on number of tracks
  if (nrow(data) == 0) {
    html <- ""
  } else {
    # format strings for web player
    na_pos <- is.na(data$copyright_url)
    data$copyright_url[na_pos] <- data$author_url[na_pos]
    # create html code
    html <- paste0("<a href=\"", data$track_url,
                   "\" class=\"player56s\" rel=\"",
                   gsub("[^[:alnum:]]", "", x), "\" ",
                   "author-url=\"", data$author_url, "\" ",
                   "attrib-image=\"", data$copyright_image, "\" ",
                   "attrib-url=\"", data$copyright_url, "\"/>",
                    data$author_name, " - ",
                    data$track_title, "</a>")
  }
  cat(html, sep = "\n")
}
