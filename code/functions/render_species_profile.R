#' Render species profile picture
#'
#' Render a species' profile picture in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @param caption \code{character} caption for image.
#'
#' @return \code{character}.
render_species_profile <- function(x, caption) {
  # parse species name to file name
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  # find url
  path <- species_data$url[species_data$species_scientific_name == x]
  # if URL is NA, then set path as default missing image
  if (is.na(path))
    path <- "assets/misc/missing-profile.png"
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
    # download the file since latex can't read images from online sources
    if (startsWith(path, "www.") || startsWith(path, "http://")) {
      new_path <- paste0("assets/profile/", x, ".", tools::file_ext(path))
      download.file(path, new_path, quiet = TRUE)
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
