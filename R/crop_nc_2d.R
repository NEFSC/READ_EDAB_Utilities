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
  shp.vect = EDABUtilities:::import_shp(shp.file) 
  use.shp = ifelse(class(shp.vect) == 'SpatVector',T,F) 
  
  if (!use.shp) stop("A valid shp.file must be provided.") 
  
  # Optimized Area Names Filtering
  if (!is.null(area.names) && !all(is.na(area.names))) { 
    shp.str <- as.data.frame(shp.vect) 
    
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
    
    shp.vect <- shp.vect[shp.vect[[target_col]][,1] %in% area.names , ] 
  }
  
  out.ls <- list() 
  
  for (i in seq_along(data.ls)) { 
    
    if (is.character(data.ls[[i]])) { 
      if (!file.exists(data.ls[[i]])) stop(sprintf("File does not exist: %s", data.ls[[i]])) 
      data.orig <- terra::rast(data.ls[[i]]) 
    } else {
      data.orig <- data.ls[[i]] 
    }
    #Check CRS
    data.crs = terra::crs(data.orig)
    shp.crs = terra::crs(shp.vect)
    
    if(!identical(data.crs,shp.crs)){
      shp.vect = terra::project(shp.vect,data.crs)
    }
    
    # ---------------------------------------------------------
    # 1. Fast Vector Shift (Align Shapefile to Raster)
    # ---------------------------------------------------------
    e_rast <- as.vector(terra::ext(data.orig)) 
    shp_crop <- shp.vect 
    
    # If raster is 0-360 but shapefile has negative longitudes
    if (e_rast["xmax"] > 180.001 && any(terra::ext(shp_crop)[1:2] < 0)) {
      # Shift the shapefile geometry 360 degrees East so it overlays on the 0-360 raster
      shp_crop <- terra::rotate(shp_crop, long = 0,split = T, left = F)
      #If raster is -180 to 180 but shapefile is 0-360
    } else if (e_rast["xmax"] <= 180.001 && any(terra::ext(shp_crop)[1:2] > 180)) {
      # Shift the shapefile geometry 360 degrees West
      shp_crop <- terra::rotate(shp_crop)
    }
    
    # ---------------------------------------------------------
    # 2. Intersection Check & Cropping
    # ---------------------------------------------------------
    # Edge Case Handling: Numeric Extent intersection check (avoids SpatExtent class mismatch)[cite: 2]
    e1 <- as.vector(terra::ext(data.orig)) 
    e2 <- as.vector(terra::ext(shp_crop))  
    
    # Bounding boxes intersect if they overlap on both axes[cite: 2]
    intersects <- (e1["xmin"] <= e2["xmax"]) && (e1["xmax"] >= e2["xmin"]) &&  
      (e1["ymin"] <= e2["ymax"]) && (e1["ymax"] >= e2["ymin"]) 
    
    if (!intersects) { 
      warning(sprintf("Data extent and shapefile extent do not intersect for item %s. Skipping.", i)) 
      next 
    }
    
    # Perform the blazing fast crop on the un-rotated raster
    data.crop <- terra::crop(data.orig, shp_crop) 
    
    # ---------------------------------------------------------
    # 3. Post-Crop Standardize
    # ---------------------------------------------------------
    # Convert longitude on the tiny cropped raster instead of the massive global one
    data.crop <- EDABUtilities::convert_2d_longitude_gridded(data.crop)[[1]]
    
    # ---------------------------------------------------------
    # 4. Write Output
    # ---------------------------------------------------------
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
