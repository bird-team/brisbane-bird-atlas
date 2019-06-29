#' Format text
#'
#' This function converts text formatted using markdown to Latex or HTML.
#' Please note that only simple formatting commands are currently accepted
#' (i.e. commands for expressing text in italic or bold font).
#'
#' @param x \code{character} containing text in markdown format.
#'
#' @param format \code{character} name of output format. Available options
#'   are: \code{"html"}, \code{"latex"}, or \code{"auto"}. Defaults to
#'   \code{"auto"}.
#'
#' @return \code{character} formatted text.
#'
#' @examples
#' format_text("this is _italics text_ and this is **bold text**.",
#'              format = "html")
#' format_text("this is _italics text_ and this is **bold text**.",
#'              format = "latex")
format_text <- function(x, format = "auto") {
  # assert arguments are valid
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::noNA(x),
                          assertthat::is.string(format),
                          assertthat::noNA(format),
                          format %in% c("latex", "html", "auto"))
  # if assert arguments are valid
  if (format == "auto") {
    if (isTRUE(knitr:::is_html_output())) {
      format <- "html"
    } else {
      format <- "latex"
    }
  }
  # convert text
  if (format == "latex") {
    ## italics
    x <- gsub(" _", " \\textit{", fixed = TRUE, x)
    x <- gsub("_", "}", fixed = TRUE, x)
    ## bold
    x <- gsub(" **", " \\textbf{", fixed = TRUE, x)
    x <- gsub("**", "}", fixed = TRUE, x)
    ## escape percentages
    x <- gsub("%", "\\%", fixed = TRUE, x)
  } else {
    ## italics
    x <- gsub(" _", " <i>", fixed = TRUE, x)
    x <- gsub("_", "</i>", fixed = TRUE, x)
    ## bold
    x <- gsub(" **", " <b>", fixed = TRUE, x)
    x <- gsub("**", "</b>", fixed = TRUE, x)
  }
  # return result
  x
}
