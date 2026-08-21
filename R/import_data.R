#' Imports data objects of various types and returns a spatRaster list for other functions
#'
#' This function extracts spatial raster data across specified shapefile regions and aggregates it temporally to produce timeseries summary statistics. It processes inputs by grouping them (e.g., aggregating daily layers into annual time series) and outputs either a list of summarized data frames or writes RDS files directly.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data.
#' @param var.name character. Name of variable to be subseted from dataset
#'
#' @return list of spatRasters
#' 

import_data = function(data.in, var.name = NULL){
  
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
  
  if(!is.null(var.name)){
    data.ls = lapply(data.ls,function(x){
      data.varname = terra::varnames(x)
      if(length(data.varname > 1)){
        data = x[[terra::varnames(x) == var.name]]
      }else{
        
        data.names = terra::names(data)
        data.names = data.names[grepl(var.name,data.names)]
        
        if(length(data.names) ==0){
          warning('NetCDF file does not contain any fields with var.name=',var.name)
        }else{
          
          data = subset(x,data.names)
        }
      }
    })
  }
  
  return(data.ls)
}