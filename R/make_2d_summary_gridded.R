#' Calculates summary statistics on a 2D spatial grid
#'
#' This function aggregates temporal spatial data (like daily rasters) into broader time periods (e.g., months, seasons) and calculates specified summary statistics. It can optionally mask the input to specific shapefile regions before summarization.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data to be processed.
#' @param var.name character. Variable name you wish to extract and process.
#' @param statistics character vector. Which statistic(s) to calculate (e.g., c("mean", "max")).
#' @param agg.time character. Time scale to aggregate over (e.g., "days", "months", "years", "season").
#' @param file.time character. Time scale of the input files ('daily', 'monthly', 'annual'). Default is 'annual'.
#' @param shp.file character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask the input data to. Default is NA.
#' @param area.names character vector or NULL. Names of shapefile areas you want to retain. Default is NULL.
#' @param tz character or NA. Time zone to convert dates to. No correction if NA. Default is NA.
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the center point. Default is TRUE.
#' @param output.files character vector or NULL. Full output file paths corresponding to each input file if write.out is TRUE. Default is NULL.
#' @param write.out logical. If TRUE, writes NetCDF files. If FALSE, returns a list of SpatRasterDatasets. Default is FALSE.
#'
#' @return If write.out is TRUE, writes NetCDF files to disk. If FALSE, returns a named list containing SpatRasterDatasets representing the summarized data.
#'
#' @export
make_2d_summary_gridded <- function(data.in, var.name, statistics, agg.time, file.time = 'annual', shp.file = NA, area.names = NULL, tz = NA, touches = TRUE, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in
  data.ls = EDABUtilities:::import_data(data.in)
  
  # Standardize shp.file
  shp.vect = EDABUtilities:::import_shp(shp.file)
  use.shp = ifelse(class(shp.vect) == 'SpatVector',T,F)
  
  # Robust filtering for area.names
  if (use.shp && !is.null(area.names) && !all(is.na(area.names))) {
    shp.str <- as.data.frame(shp.vect)
    valid_cols <- sapply(shp.str, function(col) all(area.names %in% col))
    if (!any(valid_cols)) stop("None of the shapefile attributes contain all provided area.names.")
    target_col <- names(valid_cols)[valid_cols][1]
    shp.vect <- shp.vect[shp.str[[target_col]] %in% area.names, ]
  }
  
  # OPTIMIZATION: Process 'daily' files into a single unified stack before the loop begins,
  # dropping convoluted split iteration logic entirely.
  if (file.time == 'daily' && length(data.ls) > 1) {
    if (all(sapply(data.ls, is.character))) {
      # Use terra to safely stack character string raster sources
      data_stack <- terra::rast(unlist(data.ls))
      # Extract time as a fallback if NC files don't possess native time layers
      file_dates <- suppressWarnings(as.Date(gsub('.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', unlist(data.ls))))
      if (!any(is.na(file_dates))) terra::time(data_stack) <- file_dates
    } else {
      data_stack <- terra::rast(data.ls)
    }
    data.ls <- list(data_stack)
  }
  
  if (file.time == 'monthly') stop('monthly files not yet implemented')
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      data <- terra::rast(data.ls[[i]])
    } else {
      data <- data.ls[[i]]
    }
    
    data <- EDABUtilities::convert_2d_longitude_gridded(data)[[1]]
    file.date <- as.Date(terra::time(data))
    
    if (!is.na(tz)) {
      file.date <- as.Date(as.POSIXct(file.date, tz = tz), tz = tz)
      terra::time(data) <- file.date
    }
    
    # OPTIMIZATION: Pre-crop to bounding box once to reduce spatial memory footprint immediately
    if (use.shp) {
      data <- terra::crop(data, shp.vect)
    }
    
    data.stat.ls <- list()
    
    for (j in seq_along(statistics)) {
      
      # OPTIMIZATION: Calculate terra::tapp summary BEFORE masking to shrink the temporal depth footprint
      if (agg.time == 'season') {
        data.month <- as.numeric(format(file.date, format = '%m'))
        data.season <- rep(1:4, each = 3)[data.month] # Simplified integer mapping
        stat_layer <- terra::tapp(data, index = data.season, fun = statistics[j])
      } else {
        stat_layer <- terra::tapp(data, index = agg.time, fun = statistics[j])
      }
      
      # OPTIMIZATION: Apply final exact polygon mask solely on the heavily reduced output summary
      if (use.shp) {
        stat_layer <- terra::mask(stat_layer, shp.vect, touches = touches)
      }
      
      data.stat.ls[[j]] <- stat_layer
    }
    
    data.stat <- terra::sds(data.stat.ls)
    names(data.stat) <- paste0(var.name, '_', statistics)
    
    if (write.out) {
      if (is.null(output.files)) stop("output.files must be provided when write.out is TRUE.")
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      terra::writeCDF(data.stat, output.files[i], overwrite = TRUE)
    } else {
      out.ls[[i]] <- data.stat
      if (is.character(data.ls[[i]])) {
        names(out.ls)[i] <- basename(data.ls[[i]])
      } else {
        names(out.ls)[i] <- paste0("summary_", i)
      }
    }
  }
  
  if (!write.out) {
    return(out.ls)  
  }
}