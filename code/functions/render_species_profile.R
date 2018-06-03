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
  path <- paste0("assets/profile/", x, ".png")
  # if image not found then default to missing picutre
  if (!file.exists(path))
    path <- "assets/profile/missing.png"
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
    out <- paste0(
"\\begin{figure}
\\centering
\\includegraphics[width=63px,height=120px]{", path, "}
\\caption{", caption, "}
\\end{figure}
")
  } else {
    out <- paste0(
"<div class=\"figure\" style=\"text-align: center\">
<img src=\"", path, "\" alt=\"", caption, "\" width=\"63px\" height=\"120px\">
<p class=\"caption\">
", caption, "
</p>
</div>
")
  }
  # print code
  cat(out, "\n")
}
