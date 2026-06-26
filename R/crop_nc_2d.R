#' Crops a 2D netCDF based on the extent of a shape file
#'
#' This function crops a 2D NetCDF file or SpatRaster to the spatial extent of a provided shapefile or specific zones within it. It standardizes inputs and can either return a list of cropped SpatRasters in memory or write them directly to new NetCDF files.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters.
#' @param shp.file string, SpatVector, or SpatRaster. The shapefile or spatial object used to crop the input data.
#' @param var.name string. Variable name you wish to extract.
#' @param area.names character vector. Optional names of specific areas within the shapefile to filter by before cropping. Default is NA.
#' @param write.out logical. If TRUE, writes a netCDF file. If FALSE, returns a list of SpatRasters. Default is FALSE.
#' @param output.files character vector. Full output file names corresponding to each input file. Required if write.out is TRUE. Default is NULL.
#'
#' @return A list of cropped SpatRasters if write.out is FALSE, or writes NetCDF files to disk if TRUE.
#' 
#' @export
crop_nc_2d <- function(data.in, shp.file, var.name, area.names = NA, write.out = FALSE, output.files = NULL) {
  
  # Data Input Standardization
  data.ls = EDABUtilities:::import_data(data.in)
  
  # Spatial Input Standardization
  if (inherits(shp.file, c("SpatVector", "SpatRaster"))) {
    shp.vect <- shp.file
    use.shp <- TRUE
  } else if (is.character(shp.file) && length(shp.file) == 1 && !is.na(shp.file)) {
    shp.vect <- terra::vect(shp.file)
    use.shp <- TRUE
  } else {
    use.shp <- FALSE
  }
  
  if (!use.shp) stop("A valid shp.file must be provided.")
  
  # Optimized Area Names Filtering
  if (!is.null(area.names) && !all(is.na(area.names))) {
    shp.str <- as.data.frame(shp.vect)
    
    # Safely find the first column that contains the requested area.names
    target_col <- NULL
    for (col in names(shp.str)) {
      if (all(area.names %in% shp.str[[col]])) {
        target_col <- col
        break
      }
    }
    
    if (is.null(target_col)) {
      stop("None of the attributes in shp.file contain all specified area.names.")
    }
    
    # Subset vector directly using terra logic
    shp.vect <- shp.vect[shp.vect[[target_col]] %in% area.names, ]
  }
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      if (!file.exists(data.ls[[i]])) stop(sprintf("File does not exist: %s", data.ls[[i]]))
      data.orig <- terra::rast(data.ls[[i]])
    } else {
      data.orig <- data.ls[[i]]
    }
    
    data.orig <- EDABUtilities::convert_2d_longitude_gridded(data.orig)[[1]]
    
    # Edge Case Handling: Numeric Extent intersection check (avoids SpatExtent class mismatch)
    e1 <- as.vector(terra::ext(data.orig))
    e2 <- as.vector(terra::ext(shp.vect))
    
    # Bounding boxes intersect if they overlap on both axes
    intersects <- (e1["xmin"] <= e2["xmax"]) && (e1["xmax"] >= e2["xmin"]) && 
      (e1["ymin"] <= e2["ymax"]) && (e1["ymax"] >= e2["ymin"])
    
    if (!intersects) {
      warning(sprintf("Data extent and shapefile extent do not intersect for item %s. Skipping.", i))
      next
    }
    
    data.crop <- terra::crop(data.orig, shp.vect)
    
    if (write.out) {
      if (is.null(output.files) || length(output.files) != length(data.ls)) {
        stop("output.files must be provided and match the length of data.in when write.out is TRUE.")
      }
      
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      terra::writeCDF(data.crop, output.files[i], varname = var.name, overwrite = TRUE)  
    } else {
      out.ls[[i]] <- data.crop
    }
  }
  
  if (write.out == FALSE) {
    if (is.character(data.in)) {
      names(out.ls) <- basename(data.in)
    } else {
      names(out.ls) <- paste0("layer_", seq_along(out.ls))
    }
    return(out.ls)  
  }
}
