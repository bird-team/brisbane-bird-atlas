#' Render species table
#'
#' Render a table for a species in the atlas.
#'
#' @param x \code{character} scientific name of species.
#'
#' @return \code{character}.
render_species_table <- function(x) {
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("/", "", x, fixed = TRUE)
  x <- gsub(" ", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  path <- paste0("assets/tables/", x, ".rds")
  if (!file.exists(path)) stop(paste(path, "does not exist"))
  x <- readRDS(path)
  if (!isTRUE(knitr:::is_html_output())) {
    # manually convert syntax to latex
    for (i in seq_len(ncol(x))) {
      x[[i]] <- gsub("^_", "\\\\textit{", x[[i]])
      x[[i]] <- gsub("_", "}", x[[i]], fixed = TRUE)
      x[[i]] <- gsub("%", "\\%", x[[i]], fixed = TRUE)
      x[[i]] <- gsub("&", "\\&", x[[i]], fixed = TRUE)
    }
  }
  x <- knitr::kable(x,
                    format = ifelse(isTRUE(knitr:::is_html_output()),
                                    "html", "latex"),
                    align = c("l", "l"), booktabs = TRUE, escape = FALSE,
                    col.names = c("Threat status", "Brisbane status"))
  x <- kableExtra::kable_styling(x, bootstrap_options = c("basic", "collapse"),
                                 latex_options = c("basic"),
                                 full_width = TRUE)
  x <- as.character(x)
  if (!isTRUE(knitr:::is_html_output()))
    x <- c(x, "\n\\vspace{0.25cm}\n")
  cat(x)
}
