#' Render species widget
#'
#' Render a species widget in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name, family, and
#'   order data. The argument to \code{data} must have the columns
#'   \code{"species_scientific_name"}, \code{"order_scientific_name"}, and
#'   \code{"family_scientific_name"}.
#'
#' @return interactive widget.
render_species_graph <- function(x, data) {
  pos <- which(data$species_scientific_name == x)[1]
  path <- paste0("assets/widgets/", data$order_scientific_name[pos], "-",
                 data$family_scientific_name[pos], "-", x, ".rds")
  stopifnot(file.exists(path))
  readRDS(path)
}
