#' Masks a 2D netCDF using a lower and upper value
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @param min.value numeric. Minimum value of var.name 
#' @param max.value numeric. maximum value of var.name
#' @param binary logical. Whether mask should be binary (0 for outside range, 1 for inside range) or the original values
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

mask_nc_2d <- function(data.in,write.out = F,output.files,shp.file,var.name,min.value,max.value,binary = F){
  
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
      data = terra::mask(data,shp.vect)  
    }
    
    data.mask = terra::clamp(data, lower = min.value, upper = max.value, values = F)
    
    if(binary){
       data.mask = (data.mask*0)+1
    }
    
    if(write.out){
      writeCDF(data.mask, output.files[i],varname = var.name,overwrite =T)
    }else{
      out.ls[[i]] = data.mask
    }
  }

  if(write.out ==F){
    return(out.ls)  
  }
  
}
