#' Table of contents
#'
#' Insert a table of contents (\code{\stdtableofcontents}).
#'
#' @return None.
table_of_contents <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\stdtableofcontents\n")
}

#' Clear page
#'
#' Insert white-space until the end of the page (\code{clearpage}).
#'
#' @return None.
clear_page <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\clearpage\n")
}

#' Main matter
#'
#' Start main matter (\code{mainmatter}).
#'
#' @return None.
main_matter <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\mainmatter\n")
}

#' Back matter
#'
#' Start back matter (\code{backmatter}).
#'
#' @return None.
back_matter <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\backmatter\n")
}

#' Index
#'
#' Print book index (\code{printindex}).
#'
#' @return None.
book_index <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\printindex\n")
}

#' Set start page
#'
#' Set current page as the starting page (\code{setcounter{page}{1}}).
#'
#' @return None.
start_page <- function() {
  if (!isTRUE(knitr:::is_html_output()))
    cat("\\setcounter{page}{1}\n")
}

#' Reference list
#'
#' Insert reference list into book (\code{bibliography}).
#'
#' @return None.
reference_list <- function() {
  if (!isTRUE(knitr:::is_html_output())) {
    cat("\\stdbibliography{references.bib}\n")
  } else {
    cat("<div id=\"refs\" class=\"references\"></div>\n")
  }
}

#' Two-column page layout
#'
#' Specify that text should be presented in a two-column layout in the offline
#' pdf version of the atlas.
#'
#' @return None.
#'
#' @name twocolumn

#' @rdname twocolumn
start_two_column_layout <- function() {
  if (!isTRUE(knitr:::is_html_output())) {
    cat("\\Begin{multicols}{2}\n")
  }
}

#' @rdname twocolumn
end_two_column_layout <- function() {
  if (!isTRUE(knitr:::is_html_output())) {
    cat("\\End{multicols}\n")
  }
}
