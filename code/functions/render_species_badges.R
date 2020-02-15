#' Render badges for external resources.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param data \code{data.frame} containing the scientific name of the
#'   species and associated audio file paths. The argument to \code{data} must
#'    have the columns \code{"species_scientific_name"} and \code{"audio_path"}.
#'
#' @return \code{character} html code.
render_species_badges <- function(x, data) {
  # exit early if species doesn't exist
  spp_index <- which(species_data$species_scientific_name == x)
  if (length(spp_index) == 0)
    return(invisible(TRUE))

  # extract data
  resource_names <- names(parameters$external_resources$names)
  resource_paths <- paste0("assets/badges/", resource_names, ".svg")
  resource_columns <-
    unlist(parameters$species$external_resource_column_names[resource_names],
           use.names = FALSE)
  resource_links <- unlist(data[spp_index, resource_columns], use.names = FALSE)

  # remove links that are NA
  is_valid <- !is.na(resource_links) & startsWith(resource_links, "http")
  resource_links <- resource_links[is_valid]
  resource_paths <- resource_paths[is_valid]

  # prepare html
  if (!any(is_valid)) {
    # if species has no badges then don't show anything
    html <- ""
  } else {
    # convert to image paths to URLs
    resource_urls <-
      paste0("https://raw.githubusercontent.com/",
             "bird-team/brisbane-bird-atlas/master/",
             resource_paths,
             "?sanitize=true")

    # prepare output
    html <- paste0("<a href=\"", unname(resource_links), "\">",
                       "<img src=\"", unname(resource_urls),
                       "\" style=\"max-width:100%;\">",
                       "</a>")
  }

  # output html code
  cat(html, sep = "\n")
}
