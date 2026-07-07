#' Calculates timeseries degree-day family statistics from spatial data
#'
#' This function calculates degree-day family statistics (like number of days or consecutive days above/below a threshold) by first summarizing spatial input data into a daily timeseries, then evaluating the timeseries against a reference value.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data to be processed.
#' @param var.name character. Variable name you wish to extract and process.
#' @param metric character. Which metric to calculate ('dd' for degree days, 'nd' for number of days, 'nd.con' for max consecutive number of days).
#' @param ref.value numeric. Reference point value for the threshold.
#' @param type character. How to use the reference point ('above', 'below', or 'raw').
#' @param shp.file character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask the input data to. Default is NA.
#' @param area.names character vector or NULL. Names of shapefile areas you want to retain. Default is NULL.
#' @param output.files character vector or NULL. Full output file paths corresponding to each input file if write.out is TRUE. Default is NULL.
#' @param write.out logical. If TRUE, writes RDS files. If FALSE, returns a list of data.frames. Default is FALSE.
#'
#' @return If write.out is TRUE, writes RDS files to disk. If FALSE, returns a list of data.frames summarized by timestep and area.
#' 
#' @export
make_2d_deg_day_ts <- function(data.in, var.name, metric, ref.value, type, shp.file = NA, area.names = NULL, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in
  data.ls = EDABUtilities:::import_data(data.in)
  
  # Standardize shp.file
  shp.vect = EDABUtilities:::import_shp(shp.file)
  use.shp = ifelse(class(shp.vect) == 'SpatVector',T,F)
  
  data.summary <- EDABUtilities::make_2d_summary_ts(data.in = data.ls,
                                     write.out = FALSE,
                                     shp.file = shp.vect,
                                     var.name = var.name,
                                     agg.time = 'days',
                                     statistics = 'mean',
                                     file.time = 'annual',
                                     area.names = area.names)
  
  # OPTIMIZATION: Combine into single dataframe for vectorized grouped processing
  summary_bound <- dplyr::bind_rows(data.summary)
  
  nd.con.fun <- function(x) {
    l <- rle(as.vector(x))
    m <- l$lengths[which(l$values %in% c(1, TRUE))]
    return(if (length(m) == 0) 0 else max(m, na.rm = TRUE))
  }
  
  # OPTIMIZATION: Boolean logic handled entirely inside summarise() to prevent empty-row crashes.
  # OPTIMIZATION: Fixed 'nd.con' grouping bug (now correctly using ls.id) and output column name bug.
  if (type == 'raw') {
    stat_out <- summary_bound |>
      dplyr::group_by(ls.id, var.name, area) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(metric = metric)
    
  } else if (type == 'above') {
    if (metric == 'dd') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = sum(value[value > ref.value], na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else if (metric == 'nd') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = sum(value > ref.value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else if (metric == 'nd.con') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = nd.con.fun(value > ref.value), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else {
      stop('metric needs to be "dd", "nd", or "nd.con"')
    }
    
  } else if (type == 'below') {
    if (metric == 'dd') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = sum(value[value < ref.value], na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else if (metric == 'nd') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = sum(value < ref.value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else if (metric == 'nd.con') {
      stat_out <- summary_bound |>
        dplyr::group_by(ls.id, var.name, area) |>
        dplyr::summarise(value = nd.con.fun(value < ref.value), .groups = "drop") |>
        dplyr::mutate(metric = metric)
      
    } else {
      stop('metric needs to be "dd", "nd", or "nd.con"')
    }
  }
  
  # Re-split dataframe to maintain the expected output structure (list of data.frames based on source)
  out.ls <- split(stat_out, factor(stat_out$ls.id, levels = unique(summary_bound$ls.id)))
  
  if (write.out) {
    if (is.null(output.files)) stop("output.files must be provided when write.out is TRUE.")
    
    for (i in seq_along(out.ls)) {
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      saveRDS(out.ls[[i]], output.files[i])
    }
  } else {
    return(out.ls)
  }
}