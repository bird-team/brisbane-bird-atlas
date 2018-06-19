#' Species table
#'
#' Create table showing important facts about a species.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   threat status information about the species. The argument to \code{data}
#'   must have the columns \code{"species_scientific_name"},
#'   \code{"species_scientific_name"}, \code{"iucn_threat_status"},
#'   \code{"national_threat_status"}, and \code{"qld_threat_status"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"} and \code{"season"}.
#'
#' @param grid_data \code{\link[raster]{RasterLayer} object containing the grid
#'   cells for displaying data on the map.
#'
#' @return \code{data.frame}.
species_table <- function(x, species_data, record_data, grid_data) {
  # Initialization
  spp_row <- which(species_data$species_scientific_name == x)
  spp_data <- record_data[record_data$species_scientific_name == x, ]
  # Main processing
  ## iucn threat status
  iucn_threat_status <- paste("_IUCN:_",
                              species_data$iucn_threat_status[spp_row])
  ## national threat status
  national_threat_status <- paste("_National:_",
                                  species_data$national_threat_status[spp_row])
  ## qld threat status
  qld_threat_status <- paste("_Queensland:_",
                             species_data$qld_threat_status[spp_row])
  ## ebird records
  ebird_records <- paste("_eBird records:_", formatC(nrow(spp_data),
                                                     big.mark = ","))
  ## atlas squares
  spp_cells <- raster::extract(grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  atlas_squares <- paste("_Atlas squares:_", length(unique(na.omit(spp_cells))))
  ## reporting rate
  record_data <- record_data[record_data$is_checklist &
                             record_data$is_fully_sampled_year &
                             record_data$is_after_start_year, , drop = FALSE]
  total_checklists <- dplyr::n_distinct(
    record_data$event[record_data$is_checklist])
  spp_checklists <- dplyr::n_distinct(
    record_data$event[record_data$is_checklist &
                      (record_data$species_scientific_name == x)])
  reporting_rate <- paste0("_Reporting rate:_ ",
                           round((spp_checklists / total_checklists) * 100),
                           "%")
  # return result
  data.frame(threat_status = c(iucn_threat_status, national_threat_status,
                               qld_threat_status),
             brisbane_status = c(ebird_records, atlas_squares, reporting_rate))
}
