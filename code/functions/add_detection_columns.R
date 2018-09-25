#' Add detection columns
#'
#' This function adds a column indicating if a species was detected in each
#' grid cell.
#'
#' @param x \code{character} species' scientific name.
#'
#' @param grid_data \code{\link[sf]{sf}} object. This must contain the
#'   following columns: \code{"id"}. It is assumed that this object only
#'   contains grid cells that are relevant for the species' of interest.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"season"}, \code{"is_checklist"},
#'   \code{"is_fully_sampled_year"}, \code{"year"} and \code{"maps"}.
#'
#' @param grid_data \code{\link[sf]{sf} object containing the grid
#'   cells for displaying data on the map.
#
#' @param minimum_required_events \code{numeric} number of records required
#'   for a grid cell to be adequately sampled for range estimates.
#'
#' @return \code{\link[sf]{sf}} object with the following additional columns:
#'  \code{"rate_all_year"}, \code{"rate_summer"}, \code{"rate_autumn"},
#'  \code{"rate_winter"}, \code{"rate_spring"}.
add_detection_columns <- function(x, grid_data, record_data,
                                  records_starting_year,
                                  minimum_required_events) {
  # prepare data for calculations
  ## define column name for detections
  n <- "Detection"
  grid_data[[n]] <- NA_real_
  ## extract relevant data
  full_rec_data <- record_data %>%
                   dplyr::filter(year >= records_starting_year,
                                 grid_id %in% grid_data$id) %>%
                   dplyr::select(species_scientific_name, event,
                                 grid_id, grid_type)
  spp_rec_data <- full_rec_data %>%
                  dplyr::filter(species_scientific_name == x)
  ### remove data for different species on the same sampling event
  full_rec_data <- full_rec_data %>%
                   dplyr::filter(!duplicated(event))
  # add in detection column
  ## count number of records in each grid cell
  full_tbl <- as.data.frame(table(full_rec_data$grid_id))
  spp_tbl <- as.data.frame(table(spp_rec_data$grid_id))
  ### coerce factors to integers (safely)
  full_tbl[[1]] <- as.integer(as.character(full_tbl[[1]]))
  spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
  ### name columns
  names(full_tbl) <- c("id", "count")
  if (ncol(spp_tbl) == 1) spp_tbl$count <- integer(0)
  names(spp_tbl) <- c("id", "count")
  ## add in indices corresponding to row in grid_data
  full_tbl$index <- match(full_tbl$id, grid_data$id)
  spp_tbl$index <- match(spp_tbl$id, grid_data$id)
  ### identify cells with inadequate numbers of events
  poorly_sampled <- full_tbl$count < minimum_required_events
  ## set poorly sampled cells as NA in rate_data[[l]]
  grid_data[[n]][full_tbl$index[poorly_sampled]] <- NA_real_
  ## remove cells with inadequate numbers of checklists
  full_tbl <- full_tbl[!poorly_sampled, , drop = FALSE]
  ### assign values
  grid_data[[n]][spp_tbl$index] <- 1
  grid_data[[n]][setdiff(full_tbl$index, spp_tbl$index)] <- 0
  # return result
  grid_data
}
