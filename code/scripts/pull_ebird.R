# Find the name of the file to download
pb_data <-
  piggyback:::pb_info(repo = "bird-team/brisbane-bird-atlas",
                      tag = "v.0.0.3")
file_name <- pb_data$file_name[[1]]

# Download the ebird data
piggyback::pb_download(file_name,
                       repo = "bird-team/brisbane-bird-atlas",
                       dest = "data/records",
                       tag = "v.0.0.3",
                       use_timestamps = FALSE)

# Rename the file
file.rename(file.path("data/records", file_name), "data/records/ebird_data.zip")
