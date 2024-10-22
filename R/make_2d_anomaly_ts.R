#' Creates an anomaly timeseries from a climatology
#'
#' descriptions
#'
#' @param data.in a dataframe or list of RDS files containing dataframes 
#' @param climatology a dataframe with the same time dimensions and area.names as data.in to act as a climatological reference
#' @param output.files character vector of full output file names corresponding to each input file
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @param return a dataframe output variable summarized by timestep for each area.names
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_2d_anomaly_ts  = function(data.in,climatology,output.files,write.out = F){

  climatology =dplyr::rename(climatology,ref.value = 'value')
  
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
    
    data.comb =data %>%
      dplyr::left_join(climatology,)%>%
      dplyr::mutate(anom.value = value - ref.value)
    
    
    if(write.out){
      saveRDS(data.comb, output.files[i])
    }else{
      out.ls[[i]] = data.comb
    }
    
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}