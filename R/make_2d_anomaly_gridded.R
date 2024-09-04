#' Provides a gridded climatology based on a reference dates
#'
#' descriptions
#'
#' @data.in Either a character vector of full input file names for a list of spatRasters
#' @climatology Either an input file name or spatRaster for the reference climatology. should be on same resolution as data.in
#' @output.files character vector of full output file names corresponding to each input file
#' @shp.file  string. Shape file you wish to crop each input file to
#' @var.name string. Variable name you wish to extract 
#' @area.names character vector. Names of shape file areas you want to summarise
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export
#' 

make_2d_anomaly_gridded = function(data.in,climatology,output.files,shp.file,var.name,area.names = NA,write.out =F){
  
  
  if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
  }
  
  if(is.character(climatology)){
    
    climatology = terra::rast(climatology)
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
    
    if(!(all(res(data) == res(climatology)) & all(ext(data) == ext(climatology)))){
      
      data = crop(mask(data,climatology),climatology)
      data = resample(data,climatology)
      
    }
    
    if(!is.na(shp.file)){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
      which.area = which(shp.str[,which.att] %in% area.names)
      
      
      data = terra::mask(data,shp.vect[which.area,])
      climatology = terra::mask(climatology,shp.vect[which.area,])
      
    }
    
    data.anom = data - climatology
    
    if(write.out){
      writeCDF(data.anom, output.files[i],varname = paste0(var.name,'_',statistic),overwrite =T)
    }else{
      out.ls[[i]] = data.anom
    }
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}