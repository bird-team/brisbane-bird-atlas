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
  ## format data
  record_data <- as.data.frame(record_data)
  years <- unique(record_data$year)
  record_data <- record_data[record_data$species_scientific_name == x, ,
                             drop = FALSE]
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
        dplyr::summarize(n = length(Year)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n / sum(n)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = n)) +
          ggplot2::geom_bar(stat = "identity", fill = "black",
                            color = "black") +
          ggplot2::xlab("") +
          ggplot2::ylab("Percentage of records (%)") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                             hjust = 1,
                                                             vjust = 0.8))

  ## reporting rate by month
  p2 <- record_data %>%
        dplyr::group_by( Month) %>%
        dplyr::summarize(n = length(Month)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n / sum(n)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = n)) +
          ggplot2::geom_bar(stat = "identity", fill = "black",
                            color = "black") +
          ggplot2::xlab("") +
          ggplot2::ylab("Percentage of records (%)") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                           hjust = 1,
                                                           vjust = 0.8))

  ## elevation by month
  p3 <- record_data %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(alt = mean(elevation, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(mean = mean(alt, na.rm = TRUE),
                      lower = quantile(alt, 0.25, names = FALSE, na.rm = TRUE),
                      upper = quantile(alt, 0.75, names = FALSE,
                                       na.rm = TRUE)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = mean)) +
          ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper)) +
          ggplot2::geom_point() +
          ggplot2::xlab("") +
          ggplot2::ylab("Elevation (m)") +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                             hjust = 1,
                                                             vjust = 0.8))

  ## assemble plot
  p <- gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
  # Exports
  ## return result
  p
}
