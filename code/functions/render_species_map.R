#' Render species map
#'
#' Render a species map in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return \code{gg} pkg{ggplot2} plot.
render_species_map <- function(x) {
  pos <- which(data$species_scientific_name == x)[1]
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/maps/", "-", x, ".png")
  stopifnot(file.exists(path))
  cat(paste0("![](", path, ")\n"))
}
