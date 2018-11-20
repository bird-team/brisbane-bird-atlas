#' Find an observer's name
#'
#' Find an observer's name from a sampling event identifier.
#'
#' @param x \code{character} sampling event identifier.
#'
#' @param n_tries \code{integer} number of times to attempt before giving up.
#'
#' @return \code{character} name of observer.
find_observer_name <- function(x, n_tries = 20) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x),
                          !is.na(x))
  # fetch web-page for checklist
  curr_n_tries <- 0
  w <- structure(list(), class = "try-error")
  while (inherits(w, "try-error") && (curr_n_tries < n_tries)) {
    w <- try(xml2::read_html(paste0("https://ebird.org/view/checklist/", x)),
             silent = TRUE)
    curr_n_tries <- curr_n_tries + 1
  }
  # if failed to fetch checklist print nice error message
  if (inherits(w, "try-error"))
    stop(paste("fetching data from ebird.org for checklist:", x))
  # extract observer name
  as.character(rvest::html_text(rvest::html_node(w, "dd strong"))[1])
}
