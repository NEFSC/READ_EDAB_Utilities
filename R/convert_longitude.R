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
  
  data_in <- data |>
    terra::rast()
  
  dat.ext = data_in |>
    terra::ext()
  
  lon.range = dat.ext[c(1,2)]
  
  if(all(lon.range >=0) && all(lon.range<=360)){
    
      #convert to -180 to 180
      return(terra::rotate(data_in))  

  }else if(all(lon.range >= -180) && all(lon.range <=180)){
    
    print('Already standard format (-180:180)')
    return(data_in)
    
  }else{
    
    error('Longitude out of range')
  }
}