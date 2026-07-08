#' Calculates summary statistics of 2D gridded data as a time series by area
#'
#' This function extracts spatial raster data across specified shapefile regions and aggregates it temporally to produce timeseries summary statisticss. It processes inputs by grouping them (e.g., aggregating daily layers into annual time series) and outputs either a list of summarized data frames or writes RDS files directly.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters representing the spatial data.
#' @param var.name character. Variable name you wish to extract.
#' @param statistics character vector. Which statistic(s) to calculate (e.g., c("mean", "max")).
#' @param agg.time character. Time scale to calculate aggregation over (e.g., "days", "months", "season", "years").
#' @param file.time character. What time scale the input files are on ('daily', 'monthly', 'annual'). Default is 'annual'.
#' @param shp.file character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask the input data to. Default is NA.
#' @param area.names character vector or NULL. Names of the areas in the shapefile to extract. Default is NULL.
#' @param tz character or NA. Time zone to convert. No correction if NA. Default is NA.
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the center point. Default is TRUE.
#' @param output.files character vector or NULL. Full output file paths corresponding to each processing group. Default is NULL.
#' @param write.out logical. If TRUE, writes RDS files. If FALSE, returns a list of data frames. Default is FALSE.
#'
#' @return If write.out is TRUE, writes RDS files to disk. If FALSE, returns a list of data frames summarized by timestep for each area.
#' 
#' @export
make_2d_summary_ts <- function(data.in, var.name, statistics, agg.time, file.time = 'annual', shp.file = NA, area.names = NULL, tz = NA, touches = TRUE, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in
  data.ls = EDABUtilities:::import_data(data.in)
  
  
  # Standardize shp.file
  shp.vect = EDABUtilities:::import_shp(shp.file)
  use.shp = ifelse(class(shp.vect) == 'SpatVector',T,F)
  
  out.ls <- list()
  
  # PRE-PROCESSING: Group inputs by year (if daily) or file (if annual)
  input_groups <- list() 
  loop_ids <- c()        
  
  if (file.time == 'daily') {
    if (all(sapply(data.ls, is.character))) {
      file_dates <- suppressWarnings(as.Date(gsub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", unlist(data.ls))))
    } else {
      file_dates <- as.Date(sapply(data.ls, function(x) terra::time(x)[1]))
    }
    
    if (any(is.na(file_dates))) stop("Could not parse dates from data.in to group by year.")
    
    file_years <- as.numeric(format(file_dates, "%Y"))
    unique_yrs <- sort(unique(file_years))
    
    for (yr in unique_yrs) {
      input_groups[[as.character(yr)]] <- which(file_years == yr)
    }
    loop_ids <- unique_yrs
    
  } else {
    input_groups <- as.list(seq_along(data.ls))
    if (all(sapply(data.ls, is.character))) {
      extracted_years <- gsub(".*?(\\d{4}).*", "\\1", basename(unlist(data.ls)))
      if (all(grepl("^\\d{4}$", extracted_years))) {
        loop_ids <- extracted_years
      } else {
        loop_ids <- basename(unlist(data.ls))
      }
    } else {
      loop_ids <- seq_along(data.ls)
    }
  }
  
  if (write.out && length(output.files) != length(input_groups)) {
    stop(paste0("Length mismatch: ", length(input_groups), " processing groups but ", length(output.files), " output files provided."))
  }
  
  
  # MAIN LOOP
  for (i in seq_along(input_groups)) {
    
    current_indices <- input_groups[[i]]
    current_ls_id <- loop_ids[i]
    
    # Simplified data loader capitalizing on data.ls standardization
    if (file.time == 'annual') {
      data <- if (is.character(data.ls[[current_indices]])) terra::rast(data.ls[[current_indices]]) else data.ls[[current_indices]]
    } else if (file.time == 'daily') {
      if (all(sapply(data.ls, is.character))) {
        files_to_load <- unlist(data.ls)[current_indices]
        data <- terra::rast(files_to_load)
        dates_subset <- suppressWarnings(as.Date(gsub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", files_to_load)))
        terra::time(data) <- dates_subset
      } else {
        r_list <- data.ls[current_indices]
        data <- terra::rast(r_list)
        dates_subset <- as.Date(sapply(r_list, function(x) terra::time(x)))
        terra::time(data) <- dates_subset
      }
    } else if (file.time == 'monthly') {
      stop('monthly files not yet implemented')
    }
    
    if(terra::crs(data) != terra::crs(shp.vect)){
      data = terra::project(data, terra::crs(shp.vect))
    }
    
    file.date <- terra::time(data)
    if (!is.na(tz)) {
      file.date <- as.Date(as.POSIXct(file.date, tz = tz), tz = tz)
      terra::time(data) <- file.date
    }
    
    if (agg.time == 'season') {
      data.season <- rep(1:4, each = 3)[as.numeric(format(file.date, format = "%m"))]
    }
    
    # Calculate Statistics 
    if (use.shp) {
      shp.str <- as.data.frame(shp.vect)
      if (!is.null(area.names) && !all(is.na(area.names))) {
        valid_cols <- sapply(shp.str, function(col) all(area.names %in% col))
        if (!any(valid_cols)) stop("None of the shapefile attributes contain all provided area.names.")
        target_col <- names(valid_cols)[valid_cols][1]
        which.area <- match(area.names, shp.str[[target_col]])
      } else {
        which.area <- NA
      }
      
      # OPTIMIZATION: Crop immediately to shapefile bounding box before ANY iterations
      data <- terra::crop(data, shp.vect)
      
      # OPTIMIZATION: Pull terra::tapp entirely out of the area loop. 
      # Execute once per statistics across the master clipped extent.
      agg_master_ls <- list()
      for (s in seq_along(statistics)) {
        if (agg.time == 'season') {
          agg_master_ls[[s]] <- terra::tapp(data, fun = statistics[s], index = data.season)
        } else {
          agg_master_ls[[s]] <- terra::tapp(data, fun = statistics[s], index = agg.time)
        }
      }
      
      
      all_area_results <- list()
      for (j in seq_along(area.names)) {
        
        area.poly <- shp.vect[which.area[j], ]
        stat_results_list <- list()
        
        for (s in seq_along(statistics)) {
          # OPTIMIZATION: We now merely crop/mask the ALREADY temporally-aggregated layer
          
          
          area.data <- terra::mask(terra::crop(agg_master_ls[[s]], area.poly), area.poly, touches = touches)
          stat_res <- terra::global(area.data, statistics[s], na.rm = TRUE)
          t.out <- if (agg.time == 'season') sort(unique(data.season)) else terra::time(agg_master_ls[[s]])
          
          stat_results_list[[s]] <- data.frame(
            time = t.out,
            agg.time = agg.time,
            ls.id = current_ls_id,
            var.name = var.name,
            statistics = statistics[s],
            area = area.names[j],
            value = stat_res[, 1]
          )
        }
        all_area_results[[j]] <- dplyr::bind_rows(stat_results_list)
      }
      data.stat.df <- dplyr::bind_rows(all_area_results)
      
    } else {
      agg.stat.ls <- list()
      for (s in seq_along(statistics)) {
        
        if (agg.time == 'season') {
          data.agg <- terra::tapp(data, fun = statistics[s], index = data.season)
          t.out <- sort(unique(data.season))
        } else {
          data.agg <- terra::tapp(data, fun = statistics[s], index = agg.time)
          t.out <- terra::time(data.agg)
        }
        
        res <- terra::global(data.agg, statistics[s], na.rm = TRUE)
        agg.stat.ls[[s]] <- data.frame(
          time = t.out,
          agg.time = agg.time,
          ls.id = current_ls_id,
          var.name = var.name,
          statistics = statistics[s],
          area = NA,
          value = res[, 1]
        )
      }
      data.stat.df <- dplyr::bind_rows(agg.stat.ls)
    }
    
    if (write.out) {
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      saveRDS(data.stat.df, output.files[i])
    } else {
      out.ls[[i]] <- data.stat.df
    }
  }
  
  if (!write.out) {
    return(out.ls)  
  }
}