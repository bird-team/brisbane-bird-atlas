#' Maximum y-axis value
#'
#' Identify the best looking y-axis value for a plot using opinionated settings.
#'
#' @param x \code{numeric} values
#'
#' @return \code{numeric} maximum value
ymax <- function(x) {
  x <- na.omit(x)
  if (max(x) < 0.75) return(NA_real_)
  if (max(x) < 1) return(1)
  if (max(x) < 5) return(5)
  if (max(x) < 10) return(10)
  if (max(x) < 50) return(plyr::round_any(max(x), 5, f = ceiling))
  return(plyr::round_any(max(x), 10, f = ceiling))
}
