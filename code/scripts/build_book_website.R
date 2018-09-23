time <- system.time({bookdown::render_book("index.Rmd",
                                           "bookdown::gitbook")})
message(paste0("Time spent building website: ",
               round(time[[3]] / 60, 1), " minutes"))
