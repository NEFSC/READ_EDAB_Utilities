#' Imports data objects of various types and returns a spatRaster list for other functions
#'
#' This function extracts spatial raster data across specified shapefile regions and aggregates it temporally to produce timeseries summary statistics. It processes inputs by grouping them (e.g., aggregating daily layers into annual time series) and outputs either a list of summarized data frames or writes RDS files directly.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data.
#'
#' @return list of spatRasters
#' 

import_data = function(data.in){
  
  if (inherits(data.in, "SpatRaster")|inherits(data.in, "SpatRasterDataset")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    if (!all(file.exists(data.in))) stop("One or more paths in data.in do not exist.")
    data.ls <- as.list(data.in)
    data.ls = lapply(data.ls, function(x) terra::rast(x))  # Convert file paths to SpatRaster objects
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "SpatRaster")|sapply(data.in, inherits, "SpatRasterDataset"))) {
    data.ls <- data.in
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "character"))) {
    data.ls <- lapply(data.in, function(x) terra::rast(x)) 
  } else {
    stop("data.in must be a file path, a vector of file paths, a single SpatRaster, or a list of SpatRasters.")
  }
  
  return(data.ls)
}