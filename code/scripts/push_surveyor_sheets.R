# Initialization
## set default options
options(stringsAsFactors = FALSE)

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
## iterate over the surveyor sheets and push them to GitHub
lapply(dir("assets/surveyor-sheets", "^.*\\.pdf$", full.names = TRUE),
       function(x) {
  piggyback::pb_upload(basename(x),
                       repo = "bird-team/brisbane-bird-atlas",
                       name = "assets-graphs.zip",
                       overwrite = TRUE,
                       tag = "v.0.0.2")
  invisible(TRUE)
})
