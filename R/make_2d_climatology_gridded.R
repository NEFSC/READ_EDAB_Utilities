#' Provides a gridded climatology based on a reference dates
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file name
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param area.names character vector. Names of shape file areas you want to summarise
#' @param start.time character. Either a date, year, julian day, or month corresponding to agg.time
#' @param stop.time  character. Either a date, year, julian day, or month corresponding to agg.time
#' @param agg.time character. Time scale to calculate climatology over (days,doy, months, or years)
#' @param statistic character. The statistic over which to calculate climatology from
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_climatology_gridded <- function(data.in,write.out = F,output.files,shp.file,var.name,area.names,start.time, stop.time,agg.time,statistic){
  
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
  
  data.time.agg.ls =list()
  for(i in 1:length(data.in)){
    
    if(is.character(data.in)){
      
      data = terra::rast(data.in[i])
      
    }else if(class(data.in[[i]])[1] == 'SpatRaster'){
      
      data = data.in[[i]]
      
    }else{
      stop('data.in needs to be either file names or spatRasters')
    } 
    
    if(use.shp){

      data.shp = terra::mask(data,shp.vect)
      data.time.agg = terra::tapp(data.shp,
                             index =agg.time,
                             fun = statistic)
    }else{
      
      
      data.time.agg = terra::tapp(data,
                              index =agg.time,
                              fun = statistic)
      
    }
    data.time = terra::time(data.time.agg)
    which.time = which(data.time>=start.time & data.time<=stop.time)  
    data.time.agg.ls[[i]] = terra::subset(data.time.agg,which.time)
  }
  
  data.stack = terra::sds(data.time.agg.ls)
  data.clim =terra::app(data.stack,statistic)
  
  if(write.out){
    terra::writeCDF(data.clim, output.files[i],varname = paste0(var.name,'_',statistic),overwrite =T)
  }else{
    return(data.clim)  
  }

}
