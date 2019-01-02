# Initialization
## set default options
options(stringsAsFactors = FALSE)

## set variables
n_tries <- 20

## set GITHUB_TOKEN if GITHUB_PAT present
## set slash symbol for printing
slash_symbol <- "/"
if (.Platform$OS.type == "windows")
  slash_symbol <- "\\"

## check that API settings configured for GitHub
if (identical(Sys.getenv("GITHUB_TOKEN"), "") &
    !identical(Sys.getenv("GITHUB_PAT"), ""))
  Sys.setenv("GITHUB_TOKEN" = Sys.getenv("GITHUB_PAT"))

if (identical(Sys.getenv("GITHUB_TOKEN"), "")) {
  stop(paste0("'", Sys.getenv("HOME"), slash_symbol, ".Renviron' does not ",
              "contain the credentials fir GitHub (i.e. GITHUB_TOKEN ",
              "variable)"))
}

# Main processing
## message
message("Starting to push surveyor sheets to storage..")

## try to delete all surveyor sheets currently stored on GitHub
result <- try(piggyback::pb_delete(
  piggyback::pb_list("bird-team/brisbane-bird-atlas",
                     tag = "v.0.0.2")$file_name,
  repo = "bird-team/brisbane-bird-atlas",
  tag = "v.0.0.2"), silent = TRUE)

## iterate over the surveyor sheets and push them to GitHub
result <- lapply(dir("assets/surveyor-sheets", "^.*\\.pdf$", full.names = TRUE),
                 function(x) {
  ### initialization
  print(x)
  counter <- 0
  u <- structure(list(), class = "try-error")
  ### attempt to delete and upload the file multiple times
  while(inherits(u, "try-error") & isTRUE(counter < n_tries)) {
    #### increment counter
    counter <- counter + 1
    #### try to delete file
    d <- try(piggyback::pb_delete(basename(x),
                                  repo = "bird-team/brisbane-bird-atlas",
                                  tag = "v.0.0.2"), silent = TRUE)
    #### try to upload file
    u <- try(piggyback::pb_upload(x,
                                 repo = "bird-team/brisbane-bird-atlas",
                                 name = basename(x),
                                 overwrite = TRUE,
                                 tag = "v.0.0.2",
                                 use_timestamps = FALSE), silent = TRUE)
  }
  ### if we have still failed after set number of times then throw error
  if (inherits(u, "try-error")) {
    cat(u)
    stop(paste("uploading file failed:", basename(x)))
  }
  ### if we succeeded, then return TRUE
  invisible(TRUE)
})

## message
message("Finished pushing surveyor sheets to storage!")
