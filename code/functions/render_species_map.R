#' Render species map
#'
#' Render a species map in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name, family, and
#'   order data. The argument to \code{data} must have the columns
#'   \code{"species_scientific_name"}, \code{"order_scientific_name"}, and
#'   \code{"family_scientific_name"}.
#'
#' @return \code{gg} pkg{ggplot2} plot.
render_species_map <- function(x, data) {
  pos <- which(data$species_scientific_name == x)[1]
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/maps/", data$order_scientific_name[pos], "-",
                 data$family_scientific_name[pos], "-", x, ".png")
  stopifnot(file.exists(path))
  cat(paste0("![](", path, ")\n"))
}
