#' Render species map
#'
#' Render a species map in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return \code{gg} pkg{ggplot2} plot.
render_species_map <- function(x) {
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/maps/", x, ".png")
  if(!file.exists(path)) stop(paste(path, "does not exist"))
  cat(paste0("![](", path, ")\n"))
}
