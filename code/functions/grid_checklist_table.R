#' Grid checklist table
#'
#' This function creates a checklist table for a sampling grid cell.
#'
#' @param x \code{integer} grid cell identifier.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   threat status information about the species. The argument to \code{data}
#'   must have the columns: \code{"species_scientific_name"},
#'   \code{"species_common_name"}, \code{"checklists_starting_year"},
#'   and \code{"surveyor_sheet"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"season"}, \code{"grid_id},
#'   \code{"is_checklist"}, \code{"is_fully_sampled_year"}, and
#'   \code{"year"},
#'
#' @return \code{data.frame} denoting the presence/absence of each species
#'   in the atlas per each grid cell.
grid_checklist_table <- function(x, species_data, record_data) {
  # assert that arguments are valid
  assertthat::assert_that(assertthat::is.number(x),
                          inherits(species_data, "data.frame"),
                          inherits(record_data, "sf"))
  # prepare record data
  grid_record_data <-
    record_data %>%
    as.data.frame() %>%
    dplyr::filter(grid_id == x,
                  species_scientific_name %in%
                    species_data$species_scientific_name[
                      species_data$surveyor_sheet_checklist]) %>%
    dplyr::select(species_scientific_name, season, year, is_checklist) %>%
    dplyr::group_by(species_scientific_name, season) %>%
    dplyr::summarize(most_recent_event = max(year),
                     is_checklist = is_checklist[which.max(year)]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(species_data, species_scientific_name,
                                   species_common_name,
                                   checklists_starting_year,
                                   records_starting_year),
                     by = "species_scientific_name") %>%
    dplyr::mutate(year_threshold =
      dplyr::if_else(is_checklist, checklists_starting_year,
                     records_starting_year)) %>%
    dplyr::filter(most_recent_event >= year_threshold)
  # make table
  out <- species_data %>%
         dplyr::filter(surveyor_sheet_checklist) %>%
         dplyr::select(species_common_name) %>%
         dplyr::mutate(summer = FALSE, spring = FALSE, winter = FALSE,
                       autumn = FALSE)
  for (i in c("summer", "autumn", "winter", "spring"))
    out[[i]][out$species_common_name %in%
             grid_record_data$species_common_name[
               grid_record_data$season == i]] <- TRUE
  # return table
  out
}
