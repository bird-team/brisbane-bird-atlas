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
    path <- "assets/misc/missing-profile.png"
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
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
