#' Provides a gridded anomaly based on a reference climatology
#'
#' This function calculates a spatial anomaly by subtracting a reference climatology from a gridded dataset. It standardizes spatial inputs, aligns resolutions, and outputs either a list of SpatRasters to the R environment or writes the results directly to disk as NetCDF files.
#'
#' @param data.in character vector, list, SpatRaster, or SpatRasterDataset. Single file path, vector of file paths, single spatial object, or list of spatial objects representing the raw data.
#' @param climatology string or SpatRaster. The reference climatology to subtract from the input data. Should ideally be on the same resolution as data.in.
#' @param var.name string. Variable name you wish to extract and write.
#' @param shp.file string, SpatVector, or SpatRaster. The shapefile or spatial object used to crop/mask the data. Default is NA.
#' @param area.names character vector. Optional names of specific areas within the shapefile to filter by before masking. Default is NA.
#' @param write.out logical. If TRUE, writes a netCDF file. If FALSE, returns a list of SpatRasters. Default is FALSE.
#' @param output.files character vector. Full output file names corresponding to each input file. Required if write.out is TRUE. Default is NULL.
#'
#' @return A list of SpatRasters containing the anomalies if write.out is FALSE, or writes NetCDF files to disk if TRUE.
#' 
#' @export
make_2d_anomaly_gridded <- function(data.in, climatology, var.name, shp.file = NA, area.names = NA, write.out = FALSE, output.files = NULL) {
  
  # --- Data Input Standardization ---
  data.ls = EDABUtilities:::import_data(data.in)
  
  # --- Spatial Input Standardization ---
  shp.vect = EDABUtilities:::import_shp(shp.file)
  use.shp = ifelse(class(shp.vect) == 'SpatVector',T,F)
  
  
  # --- Climatology Standardization ---
  if (is.character(climatology)) {
    if (!file.exists(climatology)) stop(sprintf("Climatology file does not exist: %s", climatology))
    climatology <- terra::rast(climatology)[[1]]
  }
  
  # --- Optimization: Pre-process spatial subsets and climatology OUTSIDE the loop ---
  if (use.shp) {
    if (!is.null(area.names) && !all(is.na(area.names))) {
      shp.str <- as.data.frame(shp.vect)
      
      # Safely locate the attribute column containing the area names
      target_col <- NULL
      for (col in names(shp.str)) {
        if (all(area.names %in% shp.str[[col]])) {
          target_col <- col
          break
        }
      }
      if (is.null(target_col)) stop("None of the attributes in shp.file contain all specified area.names.")
      
      # Direct spatial subsetting
      shp.vect <- shp.vect[shp.vect[[target_col]][,1] %in% area.names, ]
    }
    
    # Pre-mask climatology once to avoid doing it N times inside the loop
    climatology <- EDABUtilities::crop_nc_2d(climatology[[1]], shp.file = shp.vect,area.names = area.names,var.name = var.name  )[[1]]
  }
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      if (!file.exists(data.ls[[i]])) stop(sprintf("File does not exist: %s", data.ls[[i]]))
      data <- terra::rast(data.ls[[i]])
    } else if(inherits(data.ls[[i]], 'SpatRasterDataset')){
      is.sds = T
      data = terra::as.list(data.ls[[i]])
    }else {
      data <- data.ls[[i]]
    }
    
    data <- EDABUtilities::convert_2d_longitude_gridded(data)[[1]]
    
    # Align extents and resolutions if mismatched
    if (!(all(terra::res(data) == terra::res(climatology)) && all(terra::ext(data) == terra::ext(climatology)))) {
      climatology <- terra::crop(climatology[[1]],data)
      data <- terra::crop(terra::mask(data, climatology), climatology)
      data <- terra::resample(data, climatology)
    }
    
    if (use.shp) {
      
      data <- terra::mask(data, shp.vect)
    }
    
    # Calculate anomaly (NAs in the pre-masked climatology propagate automatically)
    data.anom <- data - climatology
    
    if(is.sds){
      data = terra::sds(data)
    }
    
    if (write.out) {
      if (is.null(output.files) || length(output.files) != length(data.ls)) {
        stop("output.files must be provided and match the length of data.in when write.out is TRUE.")
      }
      
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      terra::writeCDF(data.anom, output.files[i], varname = paste0(var.name, "_anomaly"), overwrite = TRUE)
    } else {
      out.ls[[i]] <- data.anom
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