#time1 <- system.time({bookdown::render_book("index.Rmd",
#                                            "bookdown::gitbook")})
#message(paste0("Time spent building gitbook: ",
#               round(time1[[3]] / 60, 1), " minutes"))
time2 <- system.time({bookdown::render_book("index.Rmd",
                                            "bookdown::pdf_book")})
message(paste0("Time spent building pdf_book: ",
               round(time2[[3]] / 60, 1), " minutes"))
