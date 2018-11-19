#' Grid summary table
#'
#' This function creates a summary table describing a grid cell.
#'
#' @param x \code{integer} grid cell identifier.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   threat status information about the species. The argument to \code{data}
#'   must have the columns: \code{"species_scientific_name"},
#'   \code{"species_common_name"}, \code{"checklists_starting_year"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"season"}, \code{"grid_id},
#'   \code{"is_checklist"}, \code{"is_fully_sampled_year"}, and
#'   \code{"year"},
#'
#' @return \code{data.frame} containing summary information.
grid_summary_table <- function(x, grid_data, species_data, record_data) {
  # assert that arguments are valid
  assertthat::assert_that(assertthat::is.number(x),
                          inherits(grid_data, "sf"),
                          inherits(species_data, "data.frame"),
                          inherits(record_data, "data.frame"))
  # subset grid data
  g <- as.data.frame(grid_data[i, ])
  # subset record data
  grid_record_data <-
    record_data %>%
    as.data.frame() %>%
    dplyr::filter(grid_id == x)
  # calculate metrics
  ## create data.frame with checklist data
  grid_checklist_data <-
    grid_record_data %>%
    dplyr::filter(is_checklist) %>%
    dplyr::left_join(dplyr::select(species_data,
                                   species_scientific_name,
                                   checklists_starting_year,
                                   records_starting_year),
                     by = "species_scientific_name") %>%
    dplyr::mutate(year_threshold = dplyr::if_else(is_checklist,
                                                  checklists_starting_year,
                                                  records_starting_year)) %>%
    dplyr::filter(year >= year_threshold) %>%
    dplyr::select(event, season, species_scientific_name, duration_minutes,
                  distance_km)
  ## complete checklists
  chk_complete_checklists <- c(
    dplyr::n_distinct(grid_checklist_data$event[
      grid_checklist_data$season == "summer"]),
    dplyr::n_distinct(grid_checklist_data$event[
      grid_checklist_data$season == "autumn"]),
    dplyr::n_distinct(grid_checklist_data$event[
      grid_checklist_data$season == "winter"]),
    dplyr::n_distinct(grid_checklist_data$event[
      grid_checklist_data$season == "spring"]))
  chk_complete_checklists_bold <- c(chk_complete_checklists <
                                    g$checklist_target, TRUE)
  chk_complete_checklists <- c(chk_complete_checklists,
                               sum(chk_complete_checklists))
  chk_complete_checklists[-5] <- paste0(chk_complete_checklists[-5], "/",
                                        g$checklist_target)
  chk_complete_checklists[chk_complete_checklists_bold] <- paste0(
    "\\textbf{", chk_complete_checklists[chk_complete_checklists_bold], "}")
  ## total minutes
  chk_total_minutes <- c(
    sum(grid_checklist_data$duration_minutes[
      grid_checklist_data$season == "summer"], na.rm = TRUE),
    sum(grid_checklist_data$duration_minutes[
      grid_checklist_data$season == "autumn"], na.rm = TRUE),
    sum(grid_checklist_data$duration_minutes[
      grid_checklist_data$season == "winter"], na.rm = TRUE),
    sum(grid_checklist_data$duration_minutes[
      grid_checklist_data$season == "spring"], na.rm = TRUE))
  chk_total_minutes_bold <- c(chk_total_minutes < g$minute_target, TRUE)
  chk_total_minutes <- c(chk_total_minutes, sum(chk_total_minutes))
  chk_total_minutes[-5] <- paste0(chk_total_minutes[-5], "/", g$minute_target)
  chk_total_minutes[chk_total_minutes_bold] <- paste0("\\textbf{",
    chk_total_minutes[chk_total_minutes_bold], "}")
  ## total km
  chk_total_km <- c(
    sum(grid_checklist_data$distance_km[
      grid_checklist_data$season == "summer"], na.rm = TRUE),
    sum(grid_checklist_data$distance_km[
      grid_checklist_data$season == "autumn"], na.rm = TRUE),
    sum(grid_checklist_data$distance_km[
      grid_checklist_data$season == "winter"], na.rm = TRUE),
    sum(grid_checklist_data$distance_km[
      grid_checklist_data$season == "spring"], na.rm = TRUE))
  chk_total_km_bold <- c(chk_total_km < g$km_target, TRUE)
  chk_total_km <- c(chk_total_km, sum(chk_total_km))
  chk_total_km[-5] <- paste0(chk_total_km[-5], "/", g$km_target)
  chk_total_km[chk_total_km_bold] <- paste0("\\textbf{",
    chk_total_km[chk_total_km_bold], "}")
  ## species
  chk_species <- c(
    dplyr::n_distinct(grid_checklist_data$species_scientific_name[
      grid_checklist_data$season == "summer"]),
    dplyr::n_distinct(grid_checklist_data$species_scientific_name[
      grid_checklist_data$season == "autumn"]),
    dplyr::n_distinct(grid_checklist_data$species_scientific_name[
      grid_checklist_data$season == "winter"]),
    dplyr::n_distinct(grid_checklist_data$species_scientific_name[
      grid_checklist_data$season == "spring"]))
  chk_species <- c(chk_species, sum(chk_species))
  chk_species <- as.character(chk_species)
  chk_species[length(chk_species)] <- paste0("\\textbf{",
    chk_species[length(chk_species)], "}")
  # grid cell leader board
  ## prepare data for leader board calculations
  ldr_checklist_data <-
    record_data %>%
    as.data.frame() %>%
    dplyr::filter(grid_id == x) %>%
    dplyr::left_join(dplyr::select(species_data,
                                   species_scientific_name,
                                   checklists_starting_year,
                                   records_starting_year),
                     by = "species_scientific_name") %>%
    dplyr::mutate(year_threshold = dplyr::if_else(is_checklist,
                                                  checklists_starting_year,
                                                  records_starting_year)) %>%
    dplyr::filter(year >= year_threshold) %>%
    dplyr::select(observer_id, event, is_checklist,
                  species_scientific_name) %>%
    dplyr::filter(observer_id %in%
                  {z <- .;
                   z %>%
                   dplyr::group_by(observer_id) %>%
                   dplyr::summarize(n =
                     dplyr::n_distinct(species_scientific_name)) %>%
                   dplyr::ungroup() %>%
                   dplyr::arrange(dplyr::desc(n)) %>%
                   head(5) %>%
                   `[[`("observer_id")}) %>%
    dplyr::group_by(observer_id) %>%
    dplyr::summarize(
      number_species = dplyr::n_distinct(species_scientific_name),
      number_complete_checklists = sum(is_checklist),
      number_incomplete_checklists = sum(!is_checklist)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(number_species))
  # output table
  data.frame(
    blank = c("Summer (Dec--Feb)", "Autumn (Mar--May)",
              "Winter (Jun--Aug)", "Spring (Sep--Nov)",
               "\\textbf{Total}"),
    Complete_checklists = chk_complete_checklists,
    Total_minutes = chk_total_minutes,
    Total_km = chk_total_km,
    Species = chk_species,
    blank2 = rep(""),
    blank3 = ldr_checklist_data$observer_id,
    Species2 = ldr_checklist_data$number_species,
    Complete_checklists2 = ldr_checklist_data$number_complete_checklists,
    Incomplete_checklists = ldr_checklist_data$number_incomplete_checklists)
}
