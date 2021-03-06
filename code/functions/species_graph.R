#' Plot species graphs
#'
#' Create graphs showing temporal distribution of species records.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   data indicating which graphs should be created. The argument to
#'   \code{species_data} must have the columns
#'   \code{"species_scientific_name"}, \code{"is_checklist"},
#'   \code{"year"}, \code{"is_fully_sampled_year"}, and
#'   \code{"graphs"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"year"}, \code{"month"}, and
#'   \code{"elevation"}.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_graph <- function(x, species_data, record_data) {
  # initialization
  ## determine which graphs to create
  spp_index <- which(species_data$species_scientific_name == x)
  graph_numbers <- species_data$graphs[spp_index]
  if (is.na(graph_numbers)) return(NULL)
  graph_numbers <- as.numeric(strsplit(graph_numbers, "_")[[1]])
  if (min(graph_numbers, na.rm = TRUE) < 1 ||
      max(graph_numbers, na.rm = TRUE) > 7 ||
      any(is.na(graph_numbers)))
    stop(paste0("processing ", x, "\ndata in graphs column must contain ",
                "integers between 1 and 4 separated by underscores ",
                "(e.g. 1_2_3_4"))
  ## coerce data to tabular format
  record_data <- as.data.frame(record_data)
  years <- unique(record_data$year)
  ## determine starting years for records and checklists
  checklists_starting_year <- species_data$checklists_starting_year[
                                    spp_index]
  records_starting_year <- species_data$records_starting_year[spp_index]
  ## create month and year columns
  record_data$Month <- factor(record_data$month,
                              levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec"))
  record_data$Year <- factor(record_data$year, levels = sort(years))
  # main processing
  ## vegetation
  d1 <- record_data %>%
        dplyr::filter(is_checklist, year >= checklists_starting_year) %>%
        dplyr::group_by(vegetation_class) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = (spp / total) * 100)
  p1 <- d1 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = vegetation_class,
                                               y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "white",
                          color = "black", width = 0.75) +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d1$rate))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.7))
  ## elevation by month
  d2 <- record_data %>%
        dplyr::filter(year >= records_starting_year) %>%
        dplyr::filter(species_scientific_name == x)
  if (all(!is.finite(d2$elevation)))
    stop("none of the records for this species overlap with the ",
         "elevation data")
  p2 <- d2 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = elevation)) +
        ggplot2::geom_boxplot() +
        ggplot2::xlab("") +
        ggplot2::ylab("Elevation (m)") +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d2$elevation))) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## reporting rate by month
  d3 <- record_data %>%
        dplyr::filter(is_checklist, year >= checklists_starting_year,
                      is_fully_sampled_year) %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        dplyr::mutate(rate = rate * 100)
  p3 <- d3 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "white",
                          color = "black", width = 0.75) +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d3$rate))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## counts by month
  d4 <- record_data %>%
        dplyr::filter(species_scientific_name == x, !is.na(count),
                      year >= records_starting_year)
  p4 <- d4 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = count)) +
        ggplot2::geom_boxplot() +
        ggplot2::xlab("") +
        ggplot2::ylab("Count (#)") +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d4$count))) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## breeding activity by month
  d5 <- record_data %>%
        dplyr::filter(species_scientific_name == x, is_fully_sampled_year,
                      year >= records_starting_year) %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          breeding = dplyr::n_distinct(event[is_breeding])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = breeding / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        dplyr::mutate(rate = rate * 100)
  breeding_ymax <- ymax(d5$rate)
  p5 <- d5 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "white",
                          color = "black", width = 0.75) +
        ggplot2::xlab("") +
        ggplot2::ylab("Breeding reported (%)") +
        ggplot2::scale_y_continuous(
          limits = c(0, ifelse(max(d5$rate, na.rm = TRUE) < 1e-15, 
                               100, breeding_ymax))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## reporting rate by year
  d6 <- record_data %>%
        dplyr::filter(is_checklist,
                      year >= checklists_starting_year,
                      is_fully_sampled_year) %>%
        dplyr::mutate(
          Year = factor(as.character(Year),
                        levels = sort(unique(as.character(Year))))) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        dplyr::mutate(rate = rate * 100)
  p6 <- d6 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "white",
                          color = "black", width = 0.75) +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d6$rate))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                              vjust = 0.8))
  ## assemble plot
  p <- list(p1, p2, p3, p4, p5, p6)[graph_numbers]
  if (length(p) == 1) {
    p <- p[[1]]
  } else if (length(p) == 2) {
    p <- {p[[1]] + p[[2]]}
  } else if (length(p) == 3) {
    p <- {p[[1]] + p[[2]] + p[[3]]}
  } else if (length(p) == 4) {
    p <- {p[[1]] + p[[2]]} / {p[[3]] + p[[4]]}
  } else if (length(p) == 5) {
    p <- {p[[1]] + p[[2]]} / {p[[3]] + p[[4]] + p[[5]]}
  } else {
    p <- {p[[1]] + p[[2]] + p[[3]]} / {p[[4]] + p[[5]] + p[[6]]}
  }
  # Exports
  ## return result
  p
}
