#' Render species widget
#'
#' Render a species widget in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return interactive widget.
render_species_widget <- function(x) {
  pos <- which(data$species_scientific_name == x)[1]
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/widgets/", "-", x, ".rds")
  stopifnot(file.exists(path))
  readRDS(path)
}
