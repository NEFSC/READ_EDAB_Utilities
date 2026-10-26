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
  
  # Boost terra memory limit for in-memory processing fallback
  terra::terraOptions(memfrac = 0.8)
  
  # 1. Standardized input coercion block
  data.ls = EDABUtilities:::import_data(data.in)
  
  # 2. Early error catch for output file configuration
  if (write.out && (length(output.files) != length(data.ls) || all(is.na(output.files)))) {
    stop("output.files must be provided and match the length of data.in when write.out is TRUE.")
  }
  
  # 3. Vectorized spatial processing loop
  out.ls <- lapply(seq_along(data.ls), function(i) {
    
    current_data <- data.ls[[i]]
    is_file_input <- is.character(current_data)
    
    # Fast path: File-to-file transformation via GDAL (only for rasters written to disk)
    if (is_file_input && write.out) {
      if (!file.exists(current_data)) stop(paste("File does not exist:", current_data))
      
      # Quickly read extent without loading data into memory
      temp_rast <- terra::rast(current_data)
      dat.ext <- terra::ext(temp_rast)
      
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      if (dat.ext[1] >= -0.001 && dat.ext[2] <= 360.001 && dat.ext[2] > 180.001) {
        message("Detected longitude range approx 0-360. Fast processing via GDAL warp...")
        
        # Perform fast GDAL extent shift
        sf::gdal_utils(
          util = "warp",
          source = current_data,
          destination = output.files[i],
          options = c(
            "-t_srs", "EPSG:4326", 
            "-te", "-180", "-90", "180", "90", 
            "-wo", "SOURCE_EXTRA=1000", 
            "--config", "CENTER_LONG", "0"
          )
        )
        return(terra::rast(output.files[i]))
        
      } else if (!(dat.ext[1] >= -180.001 && dat.ext[2] <= 180.001)) {
        stop("Longitude out of range. Extent is outside expected boundaries.")
      } else {
        message("Already standard format (-180:180). Copying file.")
        file.copy(current_data, output.files[i], overwrite = TRUE)
        return(terra::rast(output.files[i]))
      }
    }
    
    # ---------------------------------------------------------
    # Fallback path: In-memory terra processing
    # (used if input is already an object, or write.out is FALSE)
    # ---------------------------------------------------------
    
    if (is_file_input) {
      if (!file.exists(current_data)) stop(paste("File does not exist:", current_data))
      current_data <- current_data |> terra::rast()
    }
    
    dat.ext <- current_data |> terra::ext()
    
    if (dat.ext[1] >= -0.001 && dat.ext[2] <= 360.001 && dat.ext[2] > 180.001) {
      message("Detected longitude range approx 0-360. Converting to -180 to +180 via terra::rotate.")
      
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
