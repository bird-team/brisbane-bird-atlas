# Sampling grid data

## Overview

This directory contains the sampling grid data for Brisbane. It is used to define the sampling grid cells depicted in the maps, tables, graphs, and figures that shown in the atlas.

## Data origin

This data set is generated using code distributed with this repository (i.e. _code/create_grid.R_). It contains the following fields: ("id") a unique integer field to identify each grid cell, and a ("name") a text field containing the name of each grid cell. To remake this grid, enter the command `make grid` into the terminal---note that remaking the grid will reset all grid cell names. When remaking the grid, the size and coordinate system of the grid cells is controlled by the "grid_resolution" parameter in the _data/parameters/parameters.yml_ file. The data in the _data/study-area/study-area.zip_ file is used to determine the spatial extent of the grid.
