#' Provides a gridded climatology based on a reference dates
#'
#' descriptions
#'
#' @data.in Either a character vector of full input file names (RDS) for a list of data.frames
#' @output.files character vector of full output file name
#' @shp.file  string. Shape file you wish to crop each input file to
#' @var.name string. Variable name you wish to extract 
#' @area.names character vector. Names of shape file areas you want to summarise
#' @start.time character. Either a date, year, julian day, or month corresponding to agg.time
#' @stop.time  character. Either a date, year, julian day, or month corresponding to agg.time
#' @agg.time character. Time scale to calculate climatology over (days,doy, months, or years)
#' @statistic character. The statistic over which to calculate climatology from
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return netCDF file with same time dimensions as input file 
#' 
#' @export

make_2d_climatology_gridded <- function(data.in,write.out = F,output.files,start.time, stop.time,statistic){
  
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
    data.out =data %>% 
      dplyr::filter(time >= start.time & time <= stop.time )%>%
      dplyr::group_by(time,agg.time,var.name,statistic,area)%>%
      dplyr::summarise_at(.vars = 'value',.funs = statistic)

    if(write.out){
      saveRDS(data.out, output.files[i])
    }else{
      out.ls[[i]] = data.out
    }
    
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
}
