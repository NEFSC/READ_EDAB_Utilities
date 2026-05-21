#' Convert spatial object longitude to standard format
#'
#' This function converts the longitudinal coordinates of spatial objects from a 0-360 degree scale to a standard -180 to +180 degree scale. It accepts raw spatial objects or file paths and processes them into a standard list.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters/SpatVectors.
#' @param write.out logical. If TRUE, writes a netCDF file (requires output.files). If FALSE, returns a list of spatial objects. Default is FALSE.
#' @param output.files character vector. Paths for output files if write.out is TRUE. Default is NA.
#'
#' @return A named list of SpatRaster or SpatVector objects with standardized longitudes.
#' 
#' @export
convert_2d_longitude_gridded <- function(data.in, write.out = FALSE, output.files = NA) {
  
  # 1. Standardized input coercion block
  if (inherits(data.in, c("SpatRaster", "SpatVector"))) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    data.ls <- as.list(data.in)
  } else if (is.list(data.in) && all(sapply(data.in, function(x) inherits(x, c("SpatRaster", "SpatVector"))))) {
    data.ls <- data.in
  } else {
    stop("data.in must be a file path, a vector of file paths, a single SpatRaster/SpatVector, or a list of SpatRasters/SpatVectors.")
  }
  
  # 2. Early error catch for output file configuration
  if (write.out && (length(output.files) != length(data.ls) || all(is.na(output.files)))) {
    stop("output.files must be provided and match the length of data.in when write.out is TRUE.")
  }
  
  # 3. Vectorized spatial processing loop
  out.ls <- lapply(seq_along(data.ls), function(i) {
    
    current_data <- data.ls[[i]]
    
    # Safe coercion with file validation
    if (is.character(current_data)) {
      if (!file.exists(current_data)) stop(paste("File does not exist:", current_data))
      current_data <- current_data |> terra::rast()
    }
    
    dat.ext <- current_data |> terra::ext()
    
    # Spatial logic checks without redundant variables
    if (dat.ext[1] >= -0.001 && dat.ext[2] <= 360.001 && dat.ext[2] > 180.001) {
      message("Detected longitude range approximately 0-360. Converting to -180 to +180.")
      
      if (!terra::is.lonlat(current_data)) {
        warning("Object does not have a standard geographic (lat/lon) CRS. Rotation may fail or shift bounds unexpectedly.")
      }
      current_data <- terra::rotate(current_data)
      
    } else if (!(dat.ext[1] >= -180.001 && dat.ext[2] <= 180.001)) {
      stop("Longitude out of range. Extent is outside expected boundaries.")
    } else {
      message("Already standard format (-180:180)")
    }
    
    # Directory creation & data writing loop abstraction
    if (write.out) {
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      if (inherits(current_data, "SpatRaster")) {
        terra::writeCDF(current_data, filename = output.files[i], overwrite = TRUE)
      } else {
        terra::writeVector(current_data, filename = output.files[i], overwrite = TRUE)
      }
    }
    
    return(current_data)
  })
  
  # 4. Standardized list naming and return
  if (is.character(data.in)) {
    names(out.ls) <- basename(data.in)
  } else {
    names(out.ls) <- paste0("layer_", seq_along(out.ls))
  }
  
  if (!write.out) return(out.ls)
}
