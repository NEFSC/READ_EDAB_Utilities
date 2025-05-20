#' Provides summary statistics on 2d grid
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param file.time string. What time scale the input files are on ('daily','monthly','annual')? Assumes all monthly or annual files are on a daily timestep
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param agg.time string. Whether to aggregate over. Passed to terra::tapp (e.g. "days", "months", or "years", "season", etc.)
#' @param tz string. Time zone to convert. No correction if NA
#' @param statistics character vector. Which statistic to calculate
#' @param area.names character vector. Names of shape file areas you want to summarise
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_summary_gridded <- function(data.in,write.out = F,file.time = 'annual',output.files,shp.file,var.name,agg.time,tz = NA,statistics,touches = T, area.names){
  
  if(class(shp.file) %in% c('SpatVector','SpatRaster')){
    shp.vect = shp.file
    use.shp =T
  }else if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
    use.shp =T
  }else{
    use.shp = F
  }
  
  
  if(all(!is.na(area.names))){
    shp.str = as.data.frame(shp.vect)
    which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
    which.area =  match(area.names,shp.str[,which.att])
    shp.vect = shp.vect[which.area]  
  }
  
  #create iteration number based on file.time
  if(file.time == 'annual'){
    iter = 1:length(data.in)
  }else if(file.time == 'daily' & agg.time %in% c('years','months')){
    iter = 1
  }else{
    iter = 1:length(data.in)  
  }
    
    month.season = data.frame(month=1:12,season =rep(1:4,each =3))
    
    out.ls = list()
    for(i in iter){
      
      if(file.time == 'annual'){
        if(is.character(data.in)){
          
          data = terra::rast(data.in[i])
          
        }else if(class(data.in[[i]])[1] == 'SpatRaster'){
          
          data = data.in[[i]]
          
        }else{
          stop('data.in needs to be either file names or spatRasters')
        } 
      }else if (file.time == 'daily'){
        
        if(is.character(data.in)){
          
          data = lapply(data.in,function(x) terra::rast(x))
          file.date = as.Date(gsub( '.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', data.in))
          
        }else if(class(data.in[[i]])[1] == 'SpatRaster'){
          
          data = lapply(data.in,function(x) terra::rast(x))
          file.date = as.Date(sapply(data,function(x) terra::time(x)) / 86400,origin = '1970-01-01')
        }else{
          stop('data.in needs to be either file names or spatRasters')
        } 

        data = terra::rast(data)
        terra::time(data) = file.date
        
        
      }else if(file.time == 'monthly'){
        print('monthly files not yet implemented')
      }else{
        stop('file.time must be either annual, daily, or monthly')
      }
      
      
   
      data = EDABUtilities::convert_longitude(data)
      
      data.time = as.Date(terra::time(data))
      if(!is.na(tz)){
        data.time = as.Date(as.POSIXct(data.time,tz = tz),tz = tz)
        terra::time(data) = data.time
      }
      
      data.stat.ls = list()
      for(j in 1:length(statistics)){
        if(use.shp){
          
          data.shp = terra::crop(terra::mask(data,shp.vect,touches = touches),shp.vect)
          
          
            if(agg.time == 'season'){
              
              # data.time = as.Date(terra::time(data.shp))
              data.month = as.numeric(format(data.time,format = '%m'))
              data.season = month.season$season[data.month]
              data.stat.ls[[j]] = terra::tapp(data.shp,
                                              index =data.season,
                                              fun = statistics[j])
            }else{
              data.stat.ls[[j]] = terra::tapp(data.shp,
                                              index =agg.time,
                                              fun = statistics[j])
              }
          }else{
          
            if(agg.time == 'season'){
              # data.time = as.Date(terra::time(data))
              data.month = as.numeric(format(data.time,format = '%m'))
              data.season = month.season$season[data.month]
              data.stat.ls[[j]] = terra::tapp(data,
                                      index =data.season,
                                      fun = statistics[j])
      
            }else{
              data.stat.ls[[j]] = terra::tapp(data,
                                      index =agg.time,
                                      fun = statistics[j])
            }
          }
      }
      data.stat = terra::sds(data.stat.ls)
      names(data.stat) = paste0(var.name,'_',statistics)
      
      if(write.out){
        terra::writeCDF(data.stat, output.files[i],overwrite =T)
      }else{
        out.ls[[i]] = data.stat
      }
  }

  if(write.out ==F){
    return(out.ls)  
  }
  
}
