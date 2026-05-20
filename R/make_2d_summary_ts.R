#' Provides summary statistics of 2d gridded data as time series by area
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param file.time string. What time scale the input files are on ('daily','monthly','annual')? Assumes all monthly or annual files are on a daily timestep
#' @param output.files character vector of full output file names corresponding to each processing group (one file per year)
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param area.names character vector. Names of the areas in the shapefile to extract
#' @param var.name string. Variable name you wish to extract 
#' @param statistics character vector. Which statistic to calculate
#' @param agg.time character. Time scale to calculate climatology over (days,doy, months,season, or  years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return a dataframe output variable summarized by timestep for each area.names
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_2d_summary_ts = function(data.in, file.time, output.files, shp.file, area.names, var.name, agg.time, tz = NA, statistics, touches = TRUE, write.out = FALSE){
  
  if(inherits(shp.file, c('SpatVector','SpatRaster'))){
    shp.vect = shp.file
    use.shp = TRUE
  }else if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
    use.shp = TRUE
  }else{
    use.shp = FALSE
  }
  
  out.ls = list()
  
  # Helper function to create an NA dataframe to preserve structure on failure
  create_na_df <- function(current.id, stats.vec, areas.vec = NA) {
    if (all(is.na(areas.vec))) areas.vec <- NA
    
    expand.grid(
      time = NA,
      agg.time = agg.time,
      ls.id = current.id,
      var.name = var.name,
      statistic = stats.vec,
      area = areas.vec,
      value = NA,
      stringsAsFactors = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # PRE-PROCESSING: Group inputs by year (if daily) or file (if annual)
  # -------------------------------------------------------------------------
  
  input_groups <- list() # Will contain indices of data.in
  loop_ids <- c()        # Will contain labels for the groups (e.g. Years)
  
  if (file.time == 'daily') {
    # If daily, we must find years and group files
    if(is.character(data.in)){
      
      # Try pattern DD_MM_YYYY first as requested
      # Regex explanation: look for 2 digits, underscore, 2 digits, underscore, 4 digits
      file_dates <- suppressWarnings(as.Date(gsub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", data.in)))
      
    } else {
      # List of rasters
      file_dates <- as.Date(sapply(data.in, function(x) terra::time(x)[1]))
    }
    
    if (any(is.na(file_dates))) stop("Could not parse dates from data.in to group by year. Ensure files have DD_MM_YYYY or YYYY-MM-DD pattern.")
    
    file_years <- as.numeric(format(file_dates, "%Y"))
    unique_yrs <- sort(unique(file_years))
    
    for (yr in unique_yrs) {
      input_groups[[as.character(yr)]] <- which(file_years == yr)
    }
    loop_ids <- unique_yrs
    
  } else {
    # ANNUAL CASE
    # Extract YYYY from filename to use as ID
    input_groups <- as.list(1:length(data.in))
    
    if(is.character(data.in)){
      # Try to find 4 digits in the filename
      extracted_years <- gsub(".*?(\\d{4}).*", "\\1", basename(data.in))
      # Verify if we actually found numbers, otherwise fallback to filename
      if(all(grepl("^\\d{4}$", extracted_years))) {
        loop_ids <- extracted_years
      } else {
        loop_ids <- basename(data.in)
      }
    } else {
      loop_ids <- 1:length(data.in)
    }
  }
  
  # Check output file length match
  if (write.out && length(output.files) != length(input_groups)) {
    stop(paste0("Length mismatch: ", length(input_groups), " processing groups (years) but ", length(output.files), " output files provided."))
  }
  
  # -------------------------------------------------------------------------
  # MAIN LOOP
  # -------------------------------------------------------------------------
  
  for(i in 1:length(input_groups)){
    
    current_indices <- input_groups[[i]]
    current_ls_id <- loop_ids[i]
    
    # 1. Try to load data -----------------------------------------------------
    data <- tryCatch({
      
      if(file.time == 'annual'){
        # Load single file/raster
        idx <- current_indices # Single index
        if(is.character(data.in)){
          r <- terra::rast(data.in[idx])
        }else if(inherits(data.in[[idx]], 'SpatRaster')){
          if(inherits(data.in, 'list')) r <- data.in[[idx]] else r <- data.in
        }
        r
        
      } else if (file.time == 'daily'){
        # Load GROUP of files for this year
        if(is.character(data.in)){
          # Load only files for this year
          files_to_load <- data.in[current_indices]
          r_list <- lapply(files_to_load, terra::rast)
          
          # Re-extract dates for these specific files to set time correctly
          # We use the same logic as above to ensure consistency
          dates_subset <- suppressWarnings(as.Date(gsub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", files_to_load)))
          if(any(is.na(dates_subset))) {
            dates_subset <- suppressWarnings(as.Date(gsub('.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', files_to_load)))
          }
          
          r <- terra::rast(r_list)
          terra::time(r) <- dates_subset
          
        } else if(inherits(data.in[[1]], 'SpatRaster')){
          # List of rasters
          r_list <- data.in[current_indices]
          dates_subset <- as.Date(sapply(r_list, function(x) terra::time(x)))
          
          r <- terra::rast(r_list)
          terra::time(r) <- dates_subset
        }
        r
        
      } else if(file.time == 'monthly'){
        print('monthly files not yet implemented')
        NULL
      }
    }, error = function(e) {
      warning(paste("Error loading data for group", current_ls_id, ":", e$message))
      return(NULL)
    })
    
    # If data load failed or raster has no values, skip processing
    if(is.null(data) || terra::ncell(data) == 0) {
      data.stat.df <- create_na_df(current_ls_id, statistics, if(use.shp) area.names else NA)
      if(write.out){
        saveRDS(data.stat.df, output.files[i])
      } else {
        out.ls[[i]] = data.stat.df
      }
      next
    }
    
    file.date = terra::time(data)
    
    if(!is.na(tz)){
      file.date = as.Date(as.POSIXct(file.date, tz = tz), tz = tz)
      terra::time(data) = file.date
    }
    
    if(agg.time == 'season'){
      month.season = data.frame(month=1:12, season = rep(1:4, each = 3))
      data.month = as.numeric(format(file.date, format = "%m"))
      data.season = month.season$season[data.month]
      season.names = 1:4
    }
    
    # 2. Calculate Statistics -------------------------------------------------
    
    if(use.shp == TRUE){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str, 2, function(x) all(area.names %in% x)))
      
      if(length(which.att) == 0) {
        warning("Could not match area.names to shapefile attributes.")
        which.area <- NA
      } else {
        which.area = match(area.names, shp.str[,which.att])
      }
      
      all_area_results = list()
      
      # OPTIMIZATION: Loop over AREAS first, then STATISTICS
      for(j in 1:length(area.names)){
        
        # Try to crop and mask ONCE per area
        area.processed <- tryCatch({
          if(is.na(which.area[j])) stop("Invalid area index")
          area.poly <- shp.vect[which.area[j],]
          area.data = terra::crop(terra::mask(data, area.poly, touches = touches), area.poly)
          
          if(all(is.na(terra::values(area.data, mat=FALSE)))){
            stop("No data in area")
          }
          area.data
        }, error = function(e) return(NULL))
        
        if(is.null(area.processed)){
          all_area_results[[j]] = create_na_df(current_ls_id, statistics, area.names[j])
          next
        }
        
        stat_results_list = list()
        
        for(s in 1:length(statistics)){
          try_stat <- tryCatch({
            if(agg.time == 'season'){
              area.agg = terra::tapp(area.processed, fun = statistics[s], index = data.season)
              t.out = sort(unique(data.season))
            } else {
              area.agg = terra::tapp(area.processed, fun = statistics[s], index = agg.time)  
              t.out = terra::time(area.agg)
            }
            
            stat_res = terra::global(area.agg, statistics[s], na.rm=TRUE)
            list(time = t.out, val = stat_res[,1])
          }, error = function(e) list(time = NA, val = NA))
          
          stat_results_list[[s]] = data.frame(
            time = try_stat$time,
            agg.time = agg.time,
            ls.id = current_ls_id,
            var.name = var.name,
            statistic = statistics[s],
            area = area.names[j],
            value = try_stat$val
          )
        }
        all_area_results[[j]] = dplyr::bind_rows(stat_results_list)
      }
      data.stat.df = dplyr::bind_rows(all_area_results)
      
    } else {
      # Non-shapefile processing
      agg.stat.ls = list()
      
      for(s in 1:length(statistics)){
        stat.val <- tryCatch({
          if(all(is.na(terra::values(data, mat=FALSE)))) stop("Raster is empty")
          
          if(agg.time == 'season'){
            data.agg = terra::tapp(data, fun = statistics[s], index = data.season)
            t.out = sort(unique(data.season))
          } else {
            data.agg = terra::tapp(data, fun = statistics[s], index = agg.time)
            t.out = terra::time(data.agg)
          }
          
          res = terra::global(data.agg, statistics[s], na.rm=TRUE)
          list(time = t.out, val = res[,1])
          
        }, error = function(e){
          return(list(time = NA, val = NA))
        })
        
        agg.stat.ls[[s]] = data.frame(time = stat.val$time,
                                      agg.time = agg.time,
                                      ls.id = current_ls_id,
                                      var.name = var.name,
                                      statistic = statistics[s],
                                      area = NA,
                                      value = stat.val$val)
      }
      data.stat.df = dplyr::bind_rows(agg.stat.ls)
    }
    
    if(write.out){
      saveRDS(data.stat.df, output.files[i])
    }else{
      out.ls[[i]] = data.stat.df
    }
  }
  
  if(write.out == FALSE){
    return(out.ls)  
  }
}