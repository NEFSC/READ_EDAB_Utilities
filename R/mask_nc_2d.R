#' Masks a 2D netCDF using a lower and upper value
#'
#' This function filters 2D gridded spatial data by clamping values within a specified minimum and maximum range. It can optionally convert the valid range into a binary mask and crop the data to a specific spatial area prior to masking.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters.
#' @param var.name character. Variable name you wish to extract and process.
#' @param min.value numeric. Minimum value of the variable to retain.
#' @param max.value numeric. Maximum value of the variable to retain.
#' @param write.out logical. If TRUE, writes a netCDF file. If FALSE, returns a list of SpatRasters. Default is FALSE.
#' @param output.files character vector. Full output file names corresponding to each input item. Required if write.out is TRUE.
#' @param shp.file string, SpatVector, or SpatRaster. Shapefile to crop and mask each input file to. Default is NA.
#' @param binary logical. Whether the mask should be binary (1 for inside range, NA for outside) or retain original values. Default is FALSE.
#' @param area.names character vector. Specific area names to filter the shapefile by before cropping. Default is NA.
#'
#' @return A named list of SpatRasters if write.out is FALSE. Otherwise, writes NetCDF files and returns nothing.
#' 
#' @export
mask_nc_2d <- function(data.in, var.name, min.value, max.value, write.out = FALSE, output.files = NULL, shp.file = NA, binary = FALSE, area.names = NA) {
  
  # --- Input Normalization ---
  if (inherits(data.in, "SpatRaster")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    data.ls <- as.list(data.in)
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "SpatRaster"))) {
    data.ls <- data.in
  } else {
    stop("data.in must be a file path, a vector of file paths, a single SpatRaster, or a list of SpatRasters.")
  }
  
  # --- Output Directory Management ---
  if (write.out) {
    if (is.null(output.files) || length(output.files) != length(data.ls)) {
      stop("If write.out is TRUE, output.files must be provided and match the number of input items.")
    }
    # Optimize by evaluating and creating unique directories once
    out_dirs <- unique(dirname(output.files))
    for (d in out_dirs) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    }
  }
  
  # --- Spatial Extent Handling ---
  if (inherits(shp.file, c("SpatVector", "SpatRaster"))) {
    shp.vect <- shp.file
    use.shp <- TRUE
  } else if (is.character(shp.file) && length(shp.file) == 1 && !is.na(shp.file)) {
    shp.vect <- terra::vect(shp.file)
    use.shp <- TRUE
  } else {
    use.shp <- FALSE
  }
  
  # Filter shapefile by area names safely
  if (use.shp && !is.null(area.names) && !all(is.na(area.names))) {
    shp.vals <- terra::values(shp.vect)
    which.att <- which(apply(shp.vals, 2, function(x) any(area.names %in% x)))
    
    if (length(which.att) > 0) {
      # Safely subset polygons matching the area names
      target_col <- which.att[1]
      shp.vect <- shp.vect[shp.vals[[target_col]] %in% area.names, ]
    }
    
    # Edge case handler: Prevent cryptically failing loops if region filtering drops all polygons
    if (nrow(shp.vect) == 0) {
      stop("Filtering shp.file by area.names resulted in an empty shapefile. Ensure area.names exactly match vector attributes.")
    }
  }
  
  out.ls <- list()
  
  # --- Core Processing Loop ---
  for (i in seq_along(data.ls)) {
    
    item <- data.ls[[i]]
    data <- if (is.character(item)) terra::rast(item) else item
    
    if (use.shp) {
      data <- terra::crop(data, shp.vect)
      data <- terra::mask(data, shp.vect) 
    }
    
    # Clamp automatically sets out-of-bounds metrics to NA when values = FALSE
    data.out <- terra::clamp(data, lower = min.value, upper = max.value, values = FALSE)
    
    # Logical boolean replacement for binary targets
    if (binary) {
      data.out <- terra::ifel(!is.na(data.out), 1, NA)
    }
    
    if (write.out) {
      terra::writeCDF(data.out, output.files[i], varname = var.name, overwrite = TRUE)
    } else {
      out.ls[[i]] <- data.out
    }
  }
  
  # --- Return Handling ---
  if (!write.out) {
    if (is.character(data.in)) {
      names(out.ls) <- basename(data.in)
    } else {
      names(out.ls) <- paste0("layer_", seq_along(out.ls))
    }
    return(out.ls)  
  }
}