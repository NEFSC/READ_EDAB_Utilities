#' converts SpatRasters from 0:360 longitude to -180:180
#'
#' descriptions
#'
#' @param data SpatRaster object
#' 
#' @return a SpatRaster object
#' 
#' @export
#' 
convert_longitude = function(data){
  
  dat.ext = terra::ext(data)
  
  lon.range = dat.ext[c(1,2)]
  
  if(all(lon.range >=0) && all(lon.range<=360)){
    
      #convert to -180 to 180
      return(terra::rotate(data))  

  }else if(all(lon.range >= -180) && all(lon.range <=180)){
    
      return(data)
      print('Already standard format (-180:180)')
    
  }else{
    
    error('Longitude out of range')
  }
}