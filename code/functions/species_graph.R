#' Plot species graphs
#'
#' Create graphs showing temporal distribution of species records.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param species_data \code{data.frame} containing the scientific name and
#'   data indicating which graphs should be created. The argument to
#'   \code{species_data} must have the columns
#'   \code{"species_scientific_name"} and \code{"graphs"}.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"year"}, \code{"month"}, and
#'   \code{"elevation"}.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_graph <- function(x, species_data, record_data) {
  # initialization
  ## define function for ymax calculations
  ymax <- function(x) {
    if (max(x) < 1) return(1)
    if (max(x) < 5) return(5)
    if (max(x) < 10) return(10)
    if (max(x) < 50) return(plyr::round_any(max(x), 5, f = ceiling))
    return(plyr::round_any(max(x), 10, f = ceiling))
  }
  ## determine which graphs to create
  graph_numbers <- species_data$graphs[species_data$species_scientific_name ==
                                       x]
  graph_numbers <- as.numeric(strsplit(graph_numbers, "-")[[1]])
  if (min(graph_numbers, na.rm = TRUE) < 1 ||
      max(graph_numbers, na.rm = TRUE) > 4 ||
      any(is.na(graph_numbers)))
    stop(paste0("processing ", x, "\ndata in graphs column must contain ",
                "integers between 1 and 4 separated by dashes (e.g. 1-2-3-4"))
  ## coerce data to tabular format
  record_data <- as.data.frame(record_data)
  ## subset to valid years (i.e. where year has records in December)
  max_year <- max(record_data$year[record_data$month == "Dec"])
  record_data <- record_data[record_data$year <= max_year, , drop = FALSE]
  years <- unique(record_data$year)
  ## subset to checklist data
  record_data <- record_data[record_data$is_checklist, , drop = FALSE]
  ## create month and year columns
  record_data$Month <- factor(record_data$month,
                              levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec"))
  record_data$Year <- factor(record_data$year, levels = sort(years))
  # main processing
  ## vegetation
  d1 <- record_data %>%
        dplyr::group_by(vegetation_class) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = (spp / total) * 100)
  p1 <- d1 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = vegetation_class,
                                               y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
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
        dplyr::filter(species_scientific_name == x)
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
  ## reporting rate by year
  d3 <- record_data %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        dplyr::mutate(rate = rate * 100)
  p3 <- d3 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
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
  ## reporting rate by month
  d4 <- record_data %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        dplyr::mutate(rate = rate * 100)
  p4 <- d4 %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(limits = c(0, ymax(d4$rate))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## assemble plot
  p <- list(p1, p2, p3, p4)[graph_numbers]
  if (length(p) == 1) {
    p <- p[[1]]
  } else if (length(p) == 2) {
    p <- {p[[1]] + p[[2]]}
  } else if (length(p) == 3) {
    p <- {p[[1]] + p[[2]] + p[[3]]}
  } else {
    p <- {p[[1]] + p[[2]]} / {p[[3]] + p[[4]]}
  }
  # Exports
  ## return result
  p
}
