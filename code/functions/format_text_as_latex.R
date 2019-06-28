#' Format text as Latex
#'
#' This function converts text formatted using markdown for Latex. Please note
#' that only simple formatting commands are currently accepted (i.e.
#' commands for expressing text as italics or bold).
#'
#' @param x \code{character} containing text in markdown format.
#'
#' @return \code{character} text in Latex format.
#'
#' @examples
#' format_text_as_latex("this is _italics text_ and this is **bold text**.")
format_text_as_latex <- function(x) {
  assertthat::assert_that(assertthat::is.string(x))
  x <- gsub(" _", " \\textit{", fixed = TRUE, x)
  x <- gsub("_ ", "} ", fixed = TRUE, x)
  x <- gsub(" **", " \\textbf{", fixed = TRUE, x)
  x <- gsub("** ", "} ", fixed = TRUE, x)
  x
}
