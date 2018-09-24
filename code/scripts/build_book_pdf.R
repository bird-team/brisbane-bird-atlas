time <- system.time({
  # run render_book with invalid pandoc args to create raw markdown document
  try(bookdown::render_book("index.Rmd", "bookdown::pdf_book"), silent = TRUE)
  # manually convert raw markdown to latex
  ## replace " *" with "\textit{"
  ## replace " _" with "\textit{"
  ## replace " **" with "\textbf{"
  ## replace " __" with "\textbf{"
  ## replace "* " with "}"
  ## replace "_ " with "}"
  ## replace "** " with "}"
  ## replace "__ " with "}"
  # manually run pdflatex to create pdf
})
message(paste0("Time spent building pdf_book: ",
               round(time[[3]] / 60, 1), " minutes"))
