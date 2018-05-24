#' Create species widget
#'
#' @param x \code{character} scientific name of species.
#'
#' @param record_data \code{sf} object containing the records for the species.
#'   This object must have the following fields:
#'   \code{"species_scientific_name"} and \code{"season"}.
#'
#' @param grid_data \code{\link[raster]{RasterLayer} object containing the grid
#'   cells for displaying data on the map.
#'
#' @return interactive widget.
species_widget <- function(x, record_data, grid_data) {
  # initialization
  ## subset record data
  record_data <- record_data[record_data$species_scientific_name == x, ,
                             drop = FALSE]
  ## create raster stack to store frequency data
  layer_names <- c("summer", "autumn", "winter", "spring")
  grid_data <- raster::stack(grid_data, grid_data, grid_data, grid_data)
  names(grid_data) <- layer_names
  ## calculate frequency of records in grid cells
  cells <- raster::extract(grid_data[[1]], as(record_data[, "year"], "Spatial"),
                           cellnumbers = TRUE)[, 1]
  for (l in layer_names) {
    curr_tbl <- as.data.frame(table(cells[record_data$season == l]))
    if (nrow(curr_tbl) > 0) {
       curr_tbl[[1]] <- as.integer(as.character(curr_tbl[[1]]))
       grid_data[[l]][curr_tbl[[1]]] <- curr_tbl[[2]]
    }
  }
  ## create group names
  group_names <- c("Summer", "Autumn", "Winter", "Spring")
  names(grid_data) <- group_names
  ## create lon/lat version of records
  record_pts <- as(sf::st_transform(record_data[, "year"], 4326), "Spatial")
  # main processing
  ## initialize leaflet map
  l <- leaflet::leaflet()
  ## create palette
  palette <- leaflet::colorNumeric("viridis",
    range(c(c(raster::cellStats(grid_data, "min")),
            c(raster::cellStats(grid_data, "max")))),
    na.color = "transparent")
  ## add tiles
  l <- leaflet::addProviderTiles(l, "Esri.WorldImagery")
  ## add rasters
  for (i in group_names)
    l <- leaflet::addRasterImage(l, x = grid_data[[i]],
                              colors = palette, opacity = 0.6, group = i)
  ## add points
  l <- leaflet::addMarkers(l, lng = record_pts@coords[, 1],
                           lat = record_pts@coords[, 2], group = "Sightings",
                           clusterOptions = leaflet::markerClusterOptions())
  ## add legend
  l <- leaflet::addLegend(l, pal = palette,
                          values = na.omit(c(raster::values(grid_data))),
                          title = "Number sightings",
                          position = "topleft")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    baseGroups = group_names, overlayGroups = "Sighting locations",
    options = leaflet::layersControlOptions(collapsed = FALSE))
  # exports
  ## return widget
  l
}
