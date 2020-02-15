# Initialization
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

# Preliminary processing
## extract parameters
resource_names <- names(parameters$external_resources$names)
resource_images <-
  unlist(parameters$external_resources$logos[resource_names])
resource_text <-
  unlist(parameters$external_resources$names[resource_names])
assertthat::assert_that(all(file.exists(resource_images)),
                        assertthat::noNA(resource_text),
                        anyDuplicated(resource_names) == 0)

# Main processing
## prepare text for html
resource_text <- gsub(" ", "%20", resource_text, fixed = TRUE)
resource_text <- gsub("\n", "", resource_text)
resource_text <- gsub("-", "--", resource_text, fixed = TRUE)

## format SVGs for imgshields.io
formatted_images_text <-
  vapply(resource_images, FUN.VALUE = character(1), function(x) {
    x <- readLines(x)
    x <- x[!startsWith(x, "<?xml ") & !startsWith(x, "<!-- ")]
    base64enc::base64encode(charToRaw(paste(x, collapse="\n")))
  })

## create URLs for badges
badge_html <- vapply(seq_along(resource_text), FUN.VALUE = character(1),
                        function(i) {
  paste0("https://img.shields.io/badge/-", resource_text[i],
         "-grey.svg?logo=data:image/svg%2bxml;base64,",
         formatted_images_text[i])
})

## extract badge SVG files
badge_text <- vapply(badge_html, FUN.VALUE = character(1),
                     function(x) {
  rawToChar(curl::curl_fetch_memory(x)$content)
})

# Exports
## save badges to file
out <- lapply(seq_along(badge_text), function(i) {
  writeLines(badge_text[i], paste0("assets/badges/", resource_names[i], ".svg"))
})
