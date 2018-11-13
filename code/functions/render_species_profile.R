#' Render species profile picture
#'
#' Render a species' profile picture in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param caption \code{character} caption for image.
#'
#' @param n_tries \code{integer} number of times to attempt downloading the 
#'   image.
#
#' @return \code{character}.
render_species_profile <- function(x, caption, n_tries = 20) {
  # find url
  path <- species_data$profile_url[species_data$species_scientific_name == x]
  # parse species name to file name
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  # if URL is NA, then set path as default missing image
  if (is.na(path))
    path <- "assets/misc/missing-profile.png"
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
    # download the file since latex can't read images from online sources
    if (startsWith(path, "www.") || startsWith(path, "http://") ||
        startsWith(path, "https://") || startsWith(path, "ftp://")) {
      new_path <- paste0("assets/profile/", x, ".", tools::file_ext(path))
      # try to download the file, and if it fails attempt this a number of times
      curr_n_tries = 0
      out <- structure(list(), class="try-error")
      while(inherits(out, "try-error") & isTRUE(curr_n_tries < n_tries)) {
        out <- try(download.file(path, new_path, quiet = TRUE), silent = TRUE)
        curr_n_tries <- curr_n_tries + 1
      }
      if (inherits(out, "try-error"))
        stop(paste("downloading image:", path))
      path <- new_path
    }
    out <- paste0(
"\\begin{figure}
\\centering
\\includegraphics[width=\\textwidth,keepaspectratio=true]{", path, "}
\\caption{", caption, "}
\\end{figure}
")
  } else {
    out <- paste0(
"<div class=\"profile\" style=\"text-align: center\">
<img src=\"", path, "\" alt=\"", caption, "\" class=\"profile\">
<p class=\"caption\">
", caption, "
</p>
</div>
")
  }
  # print code
  cat(out, "\n")
}
