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
  # Initialization
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## create check list data with all check lists
  chk_data <- record_data[record_data$is_checklist,
                          c("species_scientific_name", "season", "event"),
                          drop = FALSE]
  ## create check list data with check list for species
  spp_data <- chk_data[chk_data$species_scientific_name == x ,
                       c("species_scientific_name", "season", "event"),
                       drop = FALSE]
  ## remove duplicate data entries
  chk_data <- chk_data[!duplicated(chk_data$event), , drop = FALSE]
  spp_data <- spp_data[!duplicated(spp_data$event), , drop = FALSE]
  # Preliminary processing
  ## create raster stack to store frequency data
  layer_names <- c("summer", "autumn", "winter", "spring")
  grid_data <- raster::stack(grid_data, grid_data, grid_data, grid_data)
  names(grid_data) <- layer_names
  ## calculate frequency of checklists in grid cells
  spp_cells <- raster::extract(grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(grid_data[[1]],
                               as(chk_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  for (l in layer_names) {
    # extract grid cells
    spp_tbl <- as.data.frame(table(spp_cells[spp_data$season == l]))
    chk_tbl <- as.data.frame(table(chk_cells[chk_data$season == l]))
    # skip if no checklists at all in this season for this species
    if (nrow(chk_tbl) > 0) {
      chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
      if (nrow(spp_tbl) == 0) {
        # assign zeros to calls with checklists for other species
        grid_data[[l]][chk_tbl[[1]]] <- 0
      } else {
        # assign total number of check lists to grid cells
        chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
        grid_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
        # calculate reporting rate for cells with checklists
        spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
        grid_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
                                        grid_data[[l]][spp_tbl[[1]]]
        # assign zeros to cells with checklists where thiss species wasn't
        # detected
        grid_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- 0
      }
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
    l <- leaflet::addRasterImage(l, x = grid_data[[i]], project = FALSE,
                                 colors = palette, opacity = 0.6, group = i)
  ## add polygons
  l <- leaflet::addPolygons(l, color = "black",
                           data = as(sf::st_transform(study_area_data, 4326),
                                     "Spatial"),
                           group = "Brisbane extent")
  ## add points
  l <- leaflet::addMarkers(l, lng = record_pts@coords[, 1],
                           lat = record_pts@coords[, 2], group = "Sightings",
                           clusterOptions = leaflet::markerClusterOptions())
  ## add legend
  l <- leaflet::addLegend(l, pal = palette, opacity = 1,
                          values = na.omit(c(raster::values(grid_data))),
                          title = "Number sightings",
                          position = "topleft")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    baseGroups = group_names, overlayGroups = c("Sightings", "Brisbane extent"),
    options = leaflet::layersControlOptions(collapsed = FALSE))
  # exports
  ## return widget
  l
}
