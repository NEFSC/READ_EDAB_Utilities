#' Calculates a gridded climatology from spatial data based on reference dates
#'
#' This function aggregates daily or periodic spatial data over a specified time scale (e.g., months) and calculates a summary statistic to establish a baseline climatology. It can optionally mask the data to specific shapefile regions before processing.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the data to be processed.
#' @param var.name character. Variable name you wish to extract and process.
#' @param agg.time character. Time scale to calculate climatology over (e.g., "days", "doy", "months", or "years").
#' @param statistic character. The statistic over which to calculate climatology (e.g., "mean", "max").
#' @param start.time numeric or character. The starting time value to filter the aggregated data.
#' @param stop.time numeric or character. The stopping time value to filter the aggregated data.
#' @param shp.file character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask the input data to. Default is NA.
#' @param area.names character vector or NULL. Names of shapefile areas you want to retain. Default is NULL.
#' @param output.files character vector or NULL. Full output file path(s) for the NetCDF file if write.out is TRUE. Default is NULL.
#' @param write.out logical. If TRUE, writes a netCDF file. If FALSE, returns a named list containing the climatology SpatRaster. Default is FALSE.
#'
#' @return If write.out is TRUE, writes a NetCDF file to disk. If FALSE, returns a named list containing the SpatRaster of the climatology.
#' 
#' @export
make_2d_climatology_gridded <- function(data.in, var.name, agg.time, statistic, start.time, stop.time, shp.file = NA, area.names = NULL, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in and assert file existence early
  data.ls = EDABUtilities:::import_data(data.in)
  
  # Standardize shp.file
  if (inherits(shp.file, c("SpatVector", "SpatRaster"))) {
    shp.vect <- shp.file
    use.shp <- TRUE
  } else if (is.character(shp.file) && length(shp.file) == 1 && !is.na(shp.file)) {
    shp.vect <- terra::vect(shp.file)
    use.shp <- TRUE
  } else {
    use.shp <- FALSE
  }
  
  # Robust filtering for area.names
  if (use.shp && !is.null(area.names) && !all(is.na(area.names))) {
    shp.str <- as.data.frame(shp.vect)
    valid_cols <- sapply(shp.str, function(col) all(area.names %in% col))
    
    if (!any(valid_cols)) {
      stop("None of the shapefile attributes contain all provided area.names.")
    }
    target_col <- names(valid_cols)[valid_cols][1]
    shp.vect <- shp.vect[shp.str[[target_col]] %in% area.names, ]
  }
  
  data.time.agg.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      data <- terra::rast(data.ls[[i]])
    } else {
      data <- data.ls[[i]]
    }
    
    data <- EDABUtilities::convert_2d_longitude_gridded(data)[[1]]
    
    # 1. OPTIMIZATION: Aggregate time BEFORE masking to reduce layer dimensions footprint
    data.time.agg <- terra::tapp(data, index = agg.time, fun = statistic)
    
    # 2. Subset times safely
    data.time <- terra::time(data.time.agg)
    which.time <- which(data.time >= start.time & data.time <= stop.time)
    
    if (length(which.time) == 0) {
      stop(paste("No layers matched start.time and stop.time parameters for dataset index:", i))
    }
    data.subset <- terra::subset(data.time.agg, which.time)
    
    # 3. OPTIMIZATION: Mask significantly fewer aggregated subset layers
    if (use.shp) {
      data.subset <- terra::mask(data.subset, shp.vect)
    }
    
    data.time.agg.ls[[i]] <- data.subset
  }
  
  # Stacking directly as a single multi-layered raster is often cleaner than sds for calculating app() metrics
  data.stack <- terra::sds(data.time.agg.ls)
  data.clim <- terra::app(data.stack, fun = statistic)
  
  if (write.out) {
    if (is.null(output.files)) stop("output.files must be provided when write.out is TRUE.")
    out_dir <- dirname(output.files[1])
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    terra::writeCDF(data.clim, output.files[1], varname = paste0(var.name, '_', statistic), overwrite = TRUE)
  } else {
    out.ls <- list(data.clim)
    names(out.ls) <- paste0("climatology_", statistic)
    return(out.ls)  
  }
}