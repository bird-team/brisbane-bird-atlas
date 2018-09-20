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
piggyback::pb_upload("assets/graphs.zip",
                     repo =  "bird-team/brisbane-bird-atlas",
                     name = "assets-graphs.zip",
                     overwrite = TRUE,
                     tag = "v.0.0.1")
piggyback::pb_upload("assets/tables.zip",
                     repo =  "bird-team/brisbane-bird-atlas",
                     name = "assets-tables.zip",
                     overwrite = TRUE,
                     tag = "v.0.0.1")
piggyback::pb_upload("assets/widgets.zip",
                     repo =  "bird-team/brisbane-bird-atlas",
                     name = "assets-widgets.zip",
                     overwrite = TRUE,
                     tag = "v.0.0.1")
piggyback::pb_upload("assets/maps.zip",
                     repo =  "bird-team/brisbane-bird-atlas",
                     name = "assets-maps.zip",
                     overwrite = TRUE,
                     tag = "v.0.0.1")
