#' Provides summary statistics of 2d gridded data as time series by area
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param file.time string. What time scale the input files are on ('daily','monthly','annual')? Assumes all monthly or annual files are on a daily timestep
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
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
  
  if(class(shp.file) %in% c('SpatVector','SpatRaster')){
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
  
  for(i in 1:length(data.in)){
    
    # Establish current ID for reporting
    current_ls_id = ifelse(is.character(data.in), data.in[i], i)
    
    # 1. Try to load data -----------------------------------------------------
    data <- tryCatch({
      if(file.time == 'annual'){
        if(is.character(data.in)){
          r <- terra::rast(data.in[i])
        }else if(class(data.in[[i]])[1] == 'SpatRaster'){
          if(class(data.in) == 'list'){
            r <- data.in[[i]]  
          }else{
            r <- data.in
          }
        }else{
          stop('data.in needs to be either file names or spatRasters')
        } 
        r
      } else if (file.time == 'daily'){
        if(is.character(data.in)){
          r_list <- lapply(data.in, function(x) terra::rast(x))
          f_date <- as.Date(gsub('.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', data.in))
        }else if(class(data.in[[i]])[1] == 'SpatRaster'){
          r_list <- lapply(data.in, function(x) terra::rast(x))
          f_date <- as.Date(sapply(r_list, function(x) terra::time(x)))
        }else{
          stop('data.in needs to be either file names or spatRasters')
        } 
        r <- terra::rast(r_list)
        terra::time(r) <- f_date
        r
      } else if(file.time == 'monthly'){
        print('monthly files not yet implemented')
        NULL
      } else {
        stop('file.time must be either annual, daily, or monthly')
      }
    }, error = function(e) {
      warning(paste("Error loading data for index", i, ":", e$message))
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
    
    agg.stat.ls = list()
    
    # 2. Calculate Statistics -------------------------------------------------
    for(s in 1:length(statistics)){
      
      if(use.shp == TRUE){
        
        shp.str = as.data.frame(shp.vect)
        # Handle finding the attribute column safely
        which.att = which(apply(shp.str, 2, function(x) all(area.names %in% x)))
        
        if(length(which.att) == 0) {
          warning("Could not match area.names to shapefile attributes.")
          which.area <- NA
        } else {
          which.area = match(area.names, shp.str[,which.att])
        }
        
        data.stat.area.ls = list()
        
        for(j in 1:length(area.names)){
          
          # Try to crop and mask
          area.stat.val <- tryCatch({
            
            # Check if index is valid
            if(is.na(which.area[j])) stop("Invalid area index")
            
            area.poly <- shp.vect[which.area[j],]
            
            # Crop and Mask
            area.data = terra::crop(terra::mask(data, area.poly, touches = touches), area.poly)
            
            # Check if area.data actually has values (not just NAs)
            if(all(is.na(terra::values(area.data, mat=FALSE)))){
              stop("No data in area")
            }
            
            if(agg.time == 'season'){
              area.agg = terra::tapp(area.data, fun = statistics[s], index = data.season)
              t.out = sort(unique(data.season))
            } else {
              area.agg = terra::tapp(area.data, fun = statistics[s], index = agg.time)  
              t.out = terra::time(area.agg)
            }
            
            # Calculate global stat
            stat_res = terra::global(area.agg, statistics[s], na.rm=TRUE)
            
            list(time = t.out, val = stat_res[,1])
            
          }, error = function(e) {
            # Return NA structure on error
            return(list(time = NA, val = NA))
          })
          
          data.stat.area.ls[[j]] = data.frame(time = area.stat.val$time,
                                              agg.time = agg.time,
                                              ls.id = current_ls_id,
                                              var.name = var.name,
                                              statistic = statistics[s],
                                              area = area.names[j],
                                              value = area.stat.val$val)
        }
        agg.stat.ls[[s]] = dplyr::bind_rows(data.stat.area.ls)
        
      } else {
        # Non-shapefile processing
        stat.val <- tryCatch({
          # Check if data has values
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
    }
    
    data.stat.df = dplyr::bind_rows(agg.stat.ls)
    
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