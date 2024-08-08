#' Crops a 2D netCDF based on the extent of a shape file
#'
#' descriptions
#'
#' @input.files character vector of full input file names
#' @output.files character vector of full output file names corresponding to each input file
#' @shp.file  string. Shape file you wish to crop each input file to
#' @var.name string. Variable name you wish to extract 
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

crop_nc_2d <- function(input.files,write.out = F,output.files,shp.file,var.name){
  
  if(!all(grepl('.nc',input.files))){
    stop('All input files must be netCDF files')
  }
  
  shp.vect = terra::vect(shp.file)
  
  data.out.ls = list()
  for(i in 1:length(input.files)){
    
    data.in = terra::rast(input.files[i],subds = var.name)
    
    data.crop= terra::crop(data.in,shp.vect)
    
    if(write.out){
      writeCDF(data.crop,output.files[i],varname = var.name,overwrite =T)  
    }else{
      data.out.ls[[i]] = data.crop
    }
    
  }
  
  if(write.out == F){
    return(data.out.ls)  
  }
  
}
