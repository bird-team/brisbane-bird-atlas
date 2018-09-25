#' Add reporting rate columns
#'
#' This function adds yearly and seasonal reporting rate columns to a
#' table.
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
#' @param checklists_starting_year \code{numeric} earliest permissible year
#'   for species' records.
#'
#' @param minimum_required_checklists \code{numeric} number of checklists
#'   required for a grid cell to be adequately sampled for reporting rates.
#'
#' @return \code{\link[sf]{sf}} object with the following additional columns:
#'  \code{"rate_all_year"}, \code{"rate_summer"}, \code{"rate_autumn"},
#'  \code{"rate_winter"}, \code{"rate_spring"}.
add_reporting_rate_columns <- function(x, grid_data, record_data,
                                       checklists_starting_year,
                                       minimum_required_checklists) {
  # subset record data
  ### create data set with data for all check lists
  full_chk_data <- record_data %>%
                   dplyr:::filter(is_checklist, is_fully_sampled_year,
                                  year >= checklists_starting_year,
                                  grid_id %in% grid_data$id) %>%
                   dplyr::select(species_scientific_name, season, event,
                                 grid_id, grid_type)
  ### create data set with check list data for species of interest
  spp_chk_data <- full_chk_data %>%
                  dplyr::filter(species_scientific_name == x)
  ### remove data for duplicated checklists
  full_chk_data <- full_chk_data %>% dplyr::filter(!duplicated(event))
  spp_chk_data <- spp_chk_data %>% dplyr::filter(!duplicated(event))
  # add in the reporting rate columns
  for (l in c("All year", "Summer", "Autumn", "Winter", "Spring")) {
    ## define current column to store result
    grid_data[[l]] <- NA_real_
    ## extract grid cells
    if (l == "All year") {
      full_tbl <- as.data.frame(table(full_chk_data$grid_id))
      spp_tbl <- as.data.frame(table(spp_chk_data$grid_id))
    } else {
      full_tbl <- as.data.frame(table(
                   full_chk_data$grid_id[full_chk_data$season %in% tolower(l)]))
      spp_tbl <- as.data.frame(table(
                   spp_chk_data$grid_id[spp_chk_data$season %in% tolower(l)]))
    }
    names(full_tbl) <- c("id", "count")
    if (ncol(spp_tbl) == 1) spp_tbl$count <- integer(0)
    names(spp_tbl) <- c("id", "count")
    ## coerce factors to integers (safely)
    full_tbl$id <- as.integer(as.character(full_tbl$id))
    spp_tbl$id <- as.integer(as.character(spp_tbl$id))
    ## add in indices corresponding to row in grid_data
    full_tbl$index <- match(full_tbl$id, grid_data$id)
    spp_tbl$index <- match(spp_tbl$id, grid_data$id)
    ## identify cells with inadequate numbers of checklists
    poorly_sampled <- full_tbl$count < minimum_required_checklists
    ## set poorly sampled cells as NA in rate_data[[l]]
    grid_data[[l]][full_tbl$index[poorly_sampled]] <- NA_real_
    ## remove cells with inadequate numbers of checklists
    full_tbl <- full_tbl[!poorly_sampled, , drop = FALSE]
    spp_tbl <- spp_tbl[spp_tbl$id %in% full_tbl$id, , drop = FALSE]
    ## skip if no checklists at all in this season for this species
    if (nrow(full_tbl) > 0) {
      if (nrow(spp_tbl) == 0) {
        ### assign zeros to cells with checklists for other species
        grid_data[[l]][full_tbl$index] <- 0
      } else {
        ### assign total number of check lists to grid cells
        grid_data[[l]][full_tbl$index] <- full_tbl$count
        ### calculate reporting rate for cells with checklists
        grid_data[[l]][spp_tbl$index] <- spp_tbl$count  /
                                         grid_data[[l]][spp_tbl$index]
        ### assign zeros to cells with checklists where this species wasn't
        ### detected
        grid_data[[l]][setdiff(full_tbl$index, spp_tbl$index)] <- 0
      }
    }
    ## multiply proportions by 100 to convert to percentages
    grid_data[[l]] <- grid_data[[l]] * 100
  }
  # return result
  grid_data
}
