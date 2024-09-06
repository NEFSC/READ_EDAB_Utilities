#' Provides a gridded climatology based on a reference dates
#'
#' descriptions
#'
#' @data.in Either a character vector of full input file names (RDS) for a list of data.frames
#' @output.file character. Full file name to write  output
#' @start.time character. Either a date, year, julian day, or month corresponding to agg.time
#' @stop.time  character. Either a date, year, julian day, or month corresponding to agg.time
#' @statistic character. The statistic over which to calculate climatology from
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_climatology_ts <- function(data.in,write.out = F,output.files,start.time, stop.time,statistic){
  
  out.ls = list()
  for(i in 1:length(data.in)){
    
    if(is.character(data.in)){
      
      data = readRDS(data.in[i])
      
    }else if(class(data.in[[i]])[1] == 'data.frame'){
      
      data = data.in[[i]]
      
    }else{
      stop('data.in needs to be either file names or data.frame')
    } 
    
    #aggregate spatially if necessary
    out.ls[[i]] =data %>% 
      dplyr::filter(time >= start.time & time <= stop.time )
  }
    
  
  clim.out =dplyr::bind_rows(out.ls)%>%
    dplyr::group_by(time,agg.time,var.name,statistic,area)%>%
    dplyr::summarise_at(.vars = 'value',.funs = statistic)
  
  if(write.out){
    saveRDS(clim.out, output.file)
  }else{
    return(clim.out)
  }

}
