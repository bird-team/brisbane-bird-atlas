#' Custom color numeric palette
#'
#' This function is essentially a copy of the
#' \code{\link[leaflet]{colorNumeric}} except that it has the additional
#' argument \code{outside.color} which specifies what color values should
#' receive when they are outside the bounds specified in the argument
#' to \code{domain}.
#'
#' @inheritParams leaflet::colorNumeric
#'
#' @param outside.color \code{character} color.
#'
#' @inherit leaflet::colorNumeric Details Examples Value#
color_numeric_palette <- function(palette, domain, na.color = "transparent",
                                 outside.color = "transparent", alpha = FALSE,
                                 reverse = FALSE) {
  rng <- NULL
  if (length(domain) > 0) {
      rng <- range(domain, na.rm = TRUE)
      if (!all(is.finite(rng))) {
          stop("Wasn't able to determine range of domain")
      }
  }
  pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)
  leaflet:::withColorAttr("numeric", list(na.color = na.color), function(x) {
      if (length(x) == 0 || all(is.na(x))) {
          return(pf(x))
      }
      if (is.null(rng))
          rng <- range(x, na.rm = TRUE)
      rescaled <- scales::rescale(x, from = rng)
      if (reverse) {
          rescaled <- 1 - rescaled
      }
      o <- pf(rescaled)
      if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE))
        o[rescaled < 0 | rescaled > 1] <- outside.color
      o
  })
}
