#' Imports shape file objects various types and returns a spatVector list for other functions
#'
#'
#' @param shp.file character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data.
#'
#' @return list of spatVectors
#' 

import_shp = function(shp.file){
  
  if (inherits(shp.file, c("SpatVector", "SpatRaster"))) {
    shp.vect <- shp.file
    use.shp <- TRUE
  } else if (is.character(shp.file) && length(shp.file) == 1 && !is.na(shp.file)) {
    shp.vect <- terra::vect(shp.file)
    use.shp <- TRUE
  } else {
    use.shp <- FALSE
  }
  
  if(use.shp ==T){
    if(terra::crs(shp.vect)==''){
      terra::crs(shp.vect) = "EPSG:4326"
    }
  }else{
    shp.vect = NA
  }
  
  return(shp.vect)
}