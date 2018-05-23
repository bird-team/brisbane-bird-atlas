#' Format species name as title
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name and common name
#'   of the species. The argument to \code{data} must have the columns
#'   \code{"species_common_name"} and \code{"species_scientific_name"}.
#'
#' @return \code{character} markdown formatted title.
species_title <- function(x, data) {
  paste0(data$species_common_name[which(data$species_scientific_name == x)[1]],
         "_", x, "_")
}

