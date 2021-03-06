#' Render species graph
#'
#' Render a species graph in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return \code{gg} pkg{ggplot2} plot.
render_species_graph <- function(x) {
  # exit early if no graph required
  spp_index <- which(species_data$species_scientific_name == x)
  if (is.na(species_data$graphs[spp_index]))
    return(invisible(TRUE))
  # otherwise render graph
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/graphs/", x, ifelse(isTRUE(knitr:::is_html_output()), ".jpeg", ".png"))
  if(!file.exists(path)) stop(paste(path, "does not exist"))
  # create code to render image
  if (!isTRUE(knitr:::is_html_output())) {
    out <- paste0(
"\\begin{figure}
\\centering
\\includegraphics[height=0.47\\textheight,width=\\textwidth,keepaspectratio=true]{", path, "}
\\end{figure}
")
  } else {
    out <- paste0(
"<div class=\"figure\">
<img src=\"", path, "\" class=\"figure\">
</div>
")
  }
  # print code
  cat(out, "\n")
}
