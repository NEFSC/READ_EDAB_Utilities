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
  
  if(!(class(data) %in% c('SpatRaster', 'SpatVector')) ){
    data_in <- data |>
      terra::rast()
  }else{
    data_in <- data
  }
  
  dat.ext = data_in |>
    terra::ext()
  
  xmin = dat.ext[1]
  xmax = dat.ext[2]
  
  
  if(xmin >= -0.001 && xmax <= 360.001 && xmax > 180.001){
    
    message("Detected longitude range approximately 0-360. Converting to -180 to +180.")
    # rast_converted <- terra::shift(data_in, dx = -180)
    rast_converted = terra::rotate(data_in)
    # plot(a)
    return(rast_converted)  

  }else if(xmin >= -180.001 && xmax <= 180.001){
    
    print('Already standard format (-180:180)')
    return(data_in)
    
  }else{
    
    error('Longitude out of range')
  }
}