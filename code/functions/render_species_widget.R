#' Render species widget
#'
#' Render a species widget in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return interactive widget.
render_species_widget <- function(x) {
  # exit early if no widget required
  spp_index <- which(species_data$species_scientific_name == x)
  if (is.na(species_data$maps[spp_index]))
    return(invisible(TRUE))
  # otherwise render widget
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/widgets/", x, ".rds")
  if(!file.exists(path)) stop(paste(path, "does not exist"))
  # create html widget
  dir.create("_book/assets/widgets_html", showWarnings = FALSE,
             recursive = TRUE)
  w <- readRDS(path)
  out <- paste0("assets/widgets_html/", x, ".html")
  withr::with_dir("_book", {
    htmlwidgets::saveWidget(w, out, libdir = "assets/widgets_html/lib",
                            selfcontained = FALSE)
  })
  # manually overwrite css to make text bigger in radio button legend
  css_path <- "assets/widgets_html/lib/leaflet-1.3.1/leaflet.css"
  css <- readLines(css_path)
  css <- gsub("font: 12px/1.5", "font: 16px/1.5", css, fixed = TRUE)
  writeLines(css, css_path)
  # manually overwrite css to make text bigger in other legends
  css_path <- "assets/widgets_html/lib/leafletfix-1.0.0/leafletfix.css"
  css <- readLines(css_path)
  css <- gsub("font: 14px/16px", "font: 16px/18px", css, fixed = TRUE)
  writeLines(css, css_path)
  # dump html widget into iframe to avoid being ingested by pandoc
  cat(paste0("<iframe src=\"assets/widgets_html/assets/widgets_html/", x,
             ".html\" height=\"500\" width=\"100%\" frameBorder=\"0\">",
             "Interactive maps cannot be viewed with your current browser.",
             "</iframe>"))
}
