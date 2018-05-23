#" Plot species graphs
#"
#" @param x \code{character} scientific name of species.
#"
#" @return \code{gg} pkg{ggplot2} plot.
species_graphs <- function(x, record_data) {
  # initialization
  ## format data
  x <- as.data.frame(x)
  years <- unique(x$year)
  x <- x[x$species_scientific_name == x, , drop = FALSE]
  ## create month and year columns
  x$Month <- factor(x$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec"))
  x$Year <- factor(x$year, levels = sort(years))
  # main processing
  ## reporting rate by month
  p1 <- x %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarize(n = length(Month)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n / sum(n)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = n)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab() +
          ggplot2::ylab("Percentage of records (%)") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40,
                                                             hjust = 1))
  ## elevation by month
  p2 <- x %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(elev = mean(elevation, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Month, y = elev)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab("") +
          ggplot2::ylab("Elevation (m)") +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40,
                                                             hjust = 1))
  ## reporting rate by year
  p3 <- x %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(n = length(Year)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n / sum(n)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = n)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::xlab("") +
          ggplot2::ylab("Percentage of records (%)") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40,
                                                             hjust = 1))
  ## assemble plot
  p <- gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
  # Exports
  ## return result
  p
}
