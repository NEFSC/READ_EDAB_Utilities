#' Provides a gridded summary of degree-day family statistics
#'
#' This function aggregates temporal spatial data to calculate degree-day statistics based on a reference threshold. It can optionally mask the input to specific shapefile regions before processing.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the data to be processed.
#' @param var.name character. Variable name you wish to extract and process.
#' @param statistic character. Which statistic to calculate ('dd' for degree days, 'nd' for number of days, 'nd.con' for max consecutive number of days).
#' @param ref.value numeric. Reference point value for the threshold.
#' @param type character. How to use the reference point ('above', 'below', or 'raw').
#' @param shp.file character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask the input data to. Default is NA.
#' @param area.names character vector or NULL. Names of shapefile areas you want to retain. Default is NULL.
#' @param output.files character vector or NULL. Full output file paths corresponding to each input file if write.out is TRUE. Default is NULL.
#' @param write.out logical. If TRUE, writes a netCDF file. If FALSE, returns a named list containing the SpatRasters. Default is FALSE.
#'
#' @return If write.out is TRUE, writes a NetCDF file with the same spatial dimensions as the input file. If FALSE, returns a named list of SpatRasters. 
#' 
#' @export
make_2d_deg_day_gridded_nc <- function(data.in, var.name, statistic, ref.value, type, shp.file = NA, area.names = NULL, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in and verify files
  if (inherits(data.in, "SpatRaster")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    if (!all(file.exists(data.in))) stop("One or more paths in data.in do not exist.")
    data.ls <- as.list(data.in)
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "SpatRaster"))) {
    data.ls <- data.in
  } else {
    stop("data.in must be a file path, a vector of file paths, a single SpatRaster, or a list of SpatRasters.")
  }
  
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
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      data <- terra::rast(data.ls[[i]])
    } else {
      data <- data.ls[[i]]
    }
    
    data <- EDABUtilities::convert_2d_longitude_gridded(data)[[1]]
    
    # Core statistical processing (Notice: Shapefile masking is deferred until after this reduces the stack!)
    if (type == 'raw') {
      data.stat <- sum(data, na.rm = TRUE)
      
    } else if (type == 'above') {
      if (statistic == 'dd') {
        data.temp <- terra::clamp(data, lower = ref.value, upper = Inf, value = FALSE)
        data.stat <- sum(data.temp, na.rm = TRUE)
        
      } else if (statistic == 'nd') {
        # OPTIMIZATION: Native terra boolean mapping instead of clamp manipulation
        data.stat <- sum(data > ref.value, na.rm = TRUE)
        
      } else if (statistic == 'nd.con') {
        data.temp <- data > ref.value
        data.stat <- terra::app(data.temp, fun = function(x) {
          l <- rle(as.vector(x))
          m <- l$lengths[which(l$values %in% c(1, TRUE))]
          return(if (length(m) == 0) 0 else max(m, na.rm = TRUE))
        })
      } else {
        stop('statistic needs to be "dd", "nd", or "nd.con"')
      }
      
    } else if (type == 'below') {
      if (statistic == 'dd') {
        data.temp <- terra::clamp(data, lower = -Inf, upper = ref.value, value = FALSE)
        data.stat <- sum(data.temp, na.rm = TRUE)
        
      } else if (statistic == 'nd') {
        data.stat <- sum(data < ref.value, na.rm = TRUE)
        
      } else if (statistic == 'nd.con') {
        data.temp <- data < ref.value
        data.stat <- terra::app(data.temp, fun = function(x) {
          l <- rle(as.vector(x))
          m <- l$lengths[which(l$values %in% c(1, TRUE))]
          return(if (length(m) == 0) 0 else max(m, na.rm = TRUE))
        })
      } else {
        stop('statistic needs to be "dd", "nd", or "nd.con"')
      }
    }
    
    # OPTIMIZATION: Re-apply the original grid's NA footprint cleanly 
    data.out <- terra::mask(data.stat, data[[1]])
    
    # OPTIMIZATION: Apply spatial mask on the final single aggregated layer
    if (use.shp) {
      data.out <- terra::mask(data.out, shp.vect)
    }
    
    if (write.out) {
      if (is.null(output.files)) stop("output.files must be provided when write.out is TRUE.")
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      terra::writeCDF(data.out, output.files[i], varname = paste0(var.name, '_', type, '_', ref.value, '_', statistic), overwrite = TRUE)
    } else {
      out.ls[[i]] <- data.out
      if (is.character(data.ls[[i]])) {
        names(out.ls)[i] <- basename(data.ls[[i]])
      } else {
        names(out.ls)[i] <- paste0("layer_", i)
      }
    }
  }
  
  if (!write.out) {
    return(out.ls)  
  }
}