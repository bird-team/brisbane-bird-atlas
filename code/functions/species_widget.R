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
#' @param study_area_data \code{sf} object showing the study area.
#'
#' @return interactive widget.
species_widget <- function(x, record_data, grid_data, study_area_data) {
  # Initialization
  ## remove name column in study_area_data
  study_area_data$name <- NULL
  ## create observation data with all observations per species
  obs_data <- record_data[record_data$species_scientific_name == x,
                          c("species_scientific_name", "season", "event"),
                          drop = FALSE]
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
  seasonal_grid_data <- raster::stack(grid_data, grid_data, grid_data,
                                               grid_data)
  names(seasonal_grid_data) <- layer_names
  ## calculate frequency of checklists in grid cells
  spp_cells <- raster::extract(seasonal_grid_data[[1]],
                               as(spp_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  chk_cells <- raster::extract(seasonal_grid_data[[1]],
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
        seasonal_grid_data[[l]][chk_tbl[[1]]] <- 0
      } else {
        # assign total number of check lists to grid cells
        chk_tbl[[1]] <- as.integer(as.character(chk_tbl[[1]]))
        seasonal_grid_data[[l]][chk_tbl[[1]]] <- chk_tbl[[2]]
        # calculate reporting rate for cells with checklists
        spp_tbl[[1]] <- as.integer(as.character(spp_tbl[[1]]))
        seasonal_grid_data[[l]][spp_tbl[[1]]] <- spp_tbl[[2]] /
                                        seasonal_grid_data[[l]][spp_tbl[[1]]]
        # assign zeros to cells with checklists where thiss species wasn't
        # detected
        seasonal_grid_data[[l]][setdiff(chk_tbl[[1]], spp_tbl[[1]])] <- 0
      }
    }
  }
  ## convert proportions to percentages
  seasonal_grid_data <- seasonal_grid_data * 100
  ## create group names
  group_names <- c("Summer", "Autumn", "Winter", "Spring")
  names(seasonal_grid_data) <- group_names
  ## create record grid data
  obs_cells <- raster::extract(grid_data,
                               as(obs_data[, "season"], "Spatial"),
                               cellnumbers = TRUE)[, 1]
  record_grid_data <- grid_data
  names(record_grid_data) <- "Records"
  obs_tbl <- as.data.frame(table(obs_cells))
  obs_tbl[[1]] <- as.integer(as.character(obs_tbl[[1]]))
  record_grid_data[obs_tbl[[1]]] <- obs_tbl[[2]]
  # main processing
  ## initialize leaflet map
  l <- leaflet::leaflet()
  ## create palettes
  seasonal_palette <- leaflet::colorNumeric("viridis",
    range(c(c(raster::cellStats(seasonal_grid_data, "min")),
            c(raster::cellStats(seasonal_grid_data, "max")))),
    na.color = "transparent")
  seasonal_palette_rev <- leaflet::colorNumeric("viridis",
    range(c(c(raster::cellStats(seasonal_grid_data, "min")),
            c(raster::cellStats(seasonal_grid_data, "max")))),
    na.color = "transparent", reverse = TRUE)
  record_palette <- leaflet::colorNumeric("viridis",
    range(c(c(raster::cellStats(record_grid_data, "min")),
            c(raster::cellStats(record_grid_data, "max")))),
    na.color = "transparent")
  record_palette_rev <- leaflet::colorNumeric("viridis",
    range(c(c(raster::cellStats(record_grid_data, "min")),
            c(raster::cellStats(record_grid_data, "max")))),
    na.color = "transparent", reverse = TRUE)

  ## add tiles
  l <- leaflet::addProviderTiles(l, "Esri.WorldImagery")
  ## add rasters
  l <- leaflet::addRasterImage(l, x = record_grid_data[[i]],
                               project = FALSE, colors = record_palette,
                               opacity = 0.6, group = "Records")
  for (i in group_names)
    l <- leaflet::addRasterImage(l, x = seasonal_grid_data[[i]],
                                 project = FALSE, colors = seasonal_palette,
                                 opacity = 0.6, group = i)
  ## add polygons
  l <- leaflet::addPolygons(l, color = "black",
                            data = as(sf::st_transform(study_area_data, 4326),
                                      "Spatial"),
                            group = "Brisbane extent")
  ## add layer control
  l <- leaflet::addLayersControl(l,
    overlayGroups = c("Records", "Brisbane extent", group_names),
    options = leaflet::layersControlOptions(collapsed = FALSE))
  ## add legends
  l <- leaflet::addLegend(l, pal = seasonal_palette_rev, opacity = 1,
                          values = c(raster::values(seasonal_grid_data)) %>%
                                   na.omit(),
                          title = "Rate (%)", position = "topright",
                          group = group_names[1],
                          labFormat = leaflet::labelFormat(transform = rev))
  l <- leaflet::addLegend(l, pal = record_palette_rev, opacity = 1,
                          values = c(raster::values(record_grid_data)) %>%
                                   na.omit(),
                          title = "Records",
                          group = "Records",
                          position = "topright",
                          labFormat = leaflet::labelFormat(transform = rev))
  # exports
  ## return widget
  l
}
