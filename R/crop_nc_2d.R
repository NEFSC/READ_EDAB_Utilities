#' Crops a 2D netCDF based on the extent of a shape file
#'
#' descriptions
#'
#' @param data.in character vector of full input file names
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

crop_nc_2d <- function(data.in,write.out = F,output.files,shp.file,var.name,area.names = NA){
  
  if(!all(grepl('.nc',data.in))){
    stop('All input files must be netCDF files')
  }
  
  if(is.na(area.names)){
    shp.vect = terra::vect(shp.file)  
  }else{
    shp.str = as.data.frame(shp.vect)
    which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
    which.area =  match(area.names,shp.str[,which.att])
    shp.vect = shp.vect[which.area]  
  }
  
  data.out.ls = list()
  for(i in 1:length(data.in)){
    
    # data.in = terra::rast(data.in[[i]],subds = var.name)
    data.orig = terra::rast(data.in[i])
    
    data.crop= terra::crop(data.orig,shp.vect)
    
    if(write.out){
      terra::writeCDF(data.crop,output.files[i],varname = var.name,overwrite =T)  
    }else{
      data.out.ls[[i]] = data.crop
    }
    
  }
  
  if(write.out == F){
    return(data.out.ls)  
  }
  
}
