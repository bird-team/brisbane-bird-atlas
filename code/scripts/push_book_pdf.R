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
## push zip files
piggyback::pb_upload("_book/brisbane-bird-atlas.pdf",
                     repo = "bird-team/brisbane-bird-atlas",
                     name = "brisbane-bird-atlas.pdf",
                     overwrite = TRUE,
                     tag = "v.0.0.1")
