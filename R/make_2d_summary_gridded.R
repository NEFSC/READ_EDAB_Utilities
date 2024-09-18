#' Provides summary statistics on 2d grid
#'
#' descriptions
#'
#' @data.in Either a character vector of full input file names for a list of spatRasters
#' @output.files character vector of full output file names corresponding to each input file
#' @shp.file  string. Shape file you wish to crop each input file to
#' @var.name string. Variable name you wish to extract 
#' @agg.time string. Whether to aggregate over. Passed to terra::tapp (e.g. "days", "months", or "years", "season", etc.)
#' @statistic string. Which statistic to calculate
#' @area.names character vector. Names of shape file areas you want to summarise
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_summary_gridded <- function(data.in,write.out = F,output.files,shp.file,var.name,agg.time,statistic,area.names){
  
  if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
  }
  
  out.ls = list()
  for(i in 1:length(data.in)){
    
    if(is.character(data.in)){
      
      data = terra::rast(data.in[i])
      
    }else if(class(data.in[[i]])[1] == 'SpatRaster'){
      
      data = data.in[[i]]
      
    }else{
      stop('data.in needs to be either file names or spatRasters')
    } 
    
    month.season = data.frame(month=1:12,season =rep(1:4,each =3))
    
    if(!is.na(shp.file)){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
      which.area = which(shp.str[,which.att] %in% area.names)
      
      
      data.shp = terra::mask(data,shp.vect[which.area,])
      
      if(agg.time == 'season'){
        
        data.time = as.Date(terra::time(data.shp))
        data.month = as.numeric(format(data.time,format = '%m'))
        data.season = month.season$season[data.month]
        data.stat = terra::tapp(data.shp,
                                index =data.season,
                                fun = statistic)
      }else{
        data.stat = terra::tapp(data.shp,
                                index =agg.time,
                                fun = statistic)
      }

    }else{
      
      if(agg.time == 'season'){
        data.time = as.Date(terra::time(data))
        data.month = as.numeric(format(data.time,format = '%m'))
        data.season = month.season$season[data.month]
        data.stat = terra::tapp(data,
                                index =data.season,
                                fun = statistic)

      }else{
        data.stat = terra::tapp(data,
                                index =agg.time,
                                fun = statistic)
      }

      
    }
    
    if(write.out){
      writeCDF(data.stat, output.files[i],varname = paste0(var.name,'_',statistic),overwrite =T)
    }else{
      out.ls[[i]] = data.stat
    }
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}
