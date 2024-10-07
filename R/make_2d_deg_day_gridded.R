#' Provides a gridded summary of degree-day family statistics
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param statistic string. Which statistic to calculate ('dd' for degree days, 'nd' for number of days, 'nd.con' for max consecutive number of days)
#' @param ref.value numeric. reference point value for threshold
#' @param type string. How to use reference point ('above', 'below', or 'raw')
#' @param area.names character vector. Names of shape file areas you want to summarise
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_deg_day_gridded_nc <- function(data.in,write.out = F,output.files,shp.file,var.name,statistic,ref.value,type,area.names){
  
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
    
    if(!is.na(shp.file)){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
      which.area =  match(area.names,shp.str[,which.att])
      
      
      data = terra::mask(data,shp.vect[which.area,])
    }
    
    if(type == 'raw'){
      
      data.stat = sum(data,na.rm=T)
      
    }else if (type == 'above'){
      
      if(statistic == 'dd'){
      
        data.temp = terra::clamp(data,lower = ref.value, upper = Inf,value = F)
        data.stat = sum(data.temp,na.rm=T)
        
      }else if (statistic == 'nd'){
        
        data.temp = (terra::clamp(data,lower = ref.value, upper = Inf,value =F)*0)+1
        data.stat = sum(data.temp,na.rm=T)
          
      }else if(statistic == 'nd.con'){
        
        data.temp = (terra::clamp(data,lower = ref.value, upper = Inf,value =F)*0)+1
        data.stat = terra::app(data.temp,fun = function(x){
          l = rle(x)
          m = l$lengths[which(l$values == 1)]
          
          return(ifelse(length(m) == 0, 0,max(m,na.rm=T)))
        })
      }else{
        
        warning('statistic needs to be "dd" or "nd"')
      }
      
    }else if (type == 'below'){
      
      if(statistic == 'dd'){
        
        data.temp = terra::clamp(data,lower = -Inf, upper = ref.value, value =F)
        data.stat = sum(data.temp,na.rm=T)
        
      }else if (statistic == 'nd'){
        
        data.temp = (terra::clamp(data,lower = -Inf, upper = ref.value, value =F)*0)+1
        data.stat = sum(data.temp,na.rm=T)
        
      }else if(statistic == 'nd.con'){
        
        data.temp = (terra::clamp(data,lower = -Inf, upper = ref.value, value =F)*0)+1
        data.stat = terra::app(data.temp,fun = function(x){
          l = rle(x)
          m = l$lengths[which(l$values == 1)]
          
          return(ifelse(length(m) == 0, 0,max(m,na.rm=T)))
        })

      }else{
        
        warning('statistic needs to be "dd" or "nd"')
      }
    }
    
    if(write.out){
      terra::writeCDF(data.stat, output.files[i],varname = paste0(var.name,'_',type,'_',ref.value,'_',statistic),overwrite =T)
    }else{
      out.ls[[i]] = data.stat
    }
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}