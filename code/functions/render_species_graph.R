#' Render species graph
#'
#' Render a species graph in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return \code{gg} pkg{ggplot2} plot.
render_species_graph <- function(x) {
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/graphs/", x, ".png")
  if(!file.exists(path)) stop(paste(path, "does not exist"))
  cat(paste0("![](", path, ")\n"))
}
