#' Plot species graphs
#'
#' Create graphs showing temporal distribution of species records.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"}, \code{"year"}, \code{"month"}, and
#'   \code{"elevation"}.
#'
#' @return \code{gg} pkg{ggplot2} plot.
species_graph <- function(x, record_data) {
  # initialization
  ## coerce data to tabular format
  record_data <- as.data.frame(record_data)
  ## extract elevational limits
  elvational_limits <- c(0, max(record_data$elevation, na.rm = TRUE))
  elvational_limits[2] <- plyr::round_any(elvational_limits[2], 10, f = ceiling)
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
  ## reporting rate by year
  p1 <- record_data %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(labels = scales::percent,
                                    limits = c(0, 1)) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                              vjust = 0.8))
  ## reporting rate by month
  p2 <- record_data %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        dplyr::mutate(rate = dplyr::if_else(is.finite(rate), rate, 0)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(labels = scales::percent,
                                    limits = c(0, 1)) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## vegetation
  p3 <- record_data %>%
        dplyr::group_by(vegetation_class) %>%
        dplyr::summarize(
          total = dplyr::n_distinct(event),
          spp = dplyr::n_distinct(event[species_scientific_name == x])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(rate = spp / total) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = vegetation_class,
                                               y = rate)) +
        ggplot2::geom_bar(stat = "identity", fill = "black",
                          color = "black") +
        ggplot2::xlab("") +
        ggplot2::ylab("Reporting rate (%)") +
        ggplot2::scale_y_continuous(labels = scales::percent,
                                    limits = c(0, 1)) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.7))
  ## elevation by month
  p4 <- record_data %>%
        dplyr::filter(species_scientific_name == x) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = elevation)) +
        ggplot2::geom_boxplot() +
        ggplot2::xlab("") +
        ggplot2::ylab("Elevation (m)") +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(limits = elvational_limits) +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.spacing.y = ggplot2::unit(0, "pt"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          vjust = 0.8))
  ## assemble plot
  p <- {p1 + p2} / {p3 + p4}
  # Exports
  ## return result
  p
}
