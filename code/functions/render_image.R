#' Render image
#'
#' This function renders an image from a file path or web address.
#'
#' @param x \code{character} path or url.
#'
#' @param caption \code{character} caption for image.
#'
#' @param n_tries \code{integer} number of times to attempt downloading the
#'   image.
#'
#' @details If the image is rendered in the web version of the book, then the
#'  argument to \code{x} is returned. Otherwise, the image is downloaded
#'  and a file path to the local copy is returned.
#'
#' @return \code{character} html or latex code to display image.
render_image <- function(x, caption = "", n_tries = 20) {
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.string(caption),
                          assertthat::is.count(n_tries))
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
    # download the file since latex can't read images from online sources
    if (startsWith(x, "www.") || startsWith(x, "http://") ||
        startsWith(x, "https://") || startsWith(x, "ftp://")) {
      new_x <- paste0("assets/misc/", basename(tempfile()), ".",
                      tools::file_ext(x))
      # try to download the file, and if it fails attempt this a number of times
      curr_n_tries <- 0
      out <- structure(list(), class = "try-error")
      while(inherits(out, "try-error") & isTRUE(curr_n_tries < n_tries)) {
        out <- try(download.file(x, new_x, quiet = TRUE), silent = TRUE)
        curr_n_tries <- curr_n_tries + 1
      }
      if (inherits(out, "try-error"))
        stop(paste("downloading image:", x))
      x <- new_x
    }
  out <- paste0(
"\\begin{figure}
\\centering
\\includegraphics[width=\\textwidth,keepaspectratio=true]{", x, "}
\\caption{", caption, "}
\\end{figure}
")
  } else {
    out <- paste0(
"<div class=\"profile\" style=\"text-align: center\">
<img src=\"", x, "\" alt=\"", caption, "\" class=\"profile\">
<p class=\"caption\">
", caption, "
</p>
</div>
")
  }
  # print code
  cat(out, "\n")
}
