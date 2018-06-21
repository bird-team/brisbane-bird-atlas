#' Breaks
#'
#' Create custom breaks for plotting.
#'
#' @param x \code{numeric} values.
#'
#' @return \code{numeric} values.
breaks <- function(x) {
  x <- na.omit(x)
  x <- c(x, ymax(x))
  x <- scales::pretty_breaks()(x)
  if (min(x) == 0)
    x[which.min(x)] <- 1
  x
}
