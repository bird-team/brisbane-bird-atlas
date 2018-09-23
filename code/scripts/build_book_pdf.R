time <- system.time({bookdown::render_book("index.Rmd",
                                           "bookdown::pdf_book")})
message(paste0("Time spent building pdf_book: ",
               round(time[[3]] / 60, 1), " minutes"))
