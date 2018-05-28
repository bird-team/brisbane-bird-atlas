#' Render species widget
#'
#' Render a species widget in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return interactive widget.
render_species_widget <- function(x) {
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/widgets/", x, ".rds")
  if(!file.exists(path)) stop(paste(path, "does not exist"))
  readRDS(path)
}
