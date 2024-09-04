#' Provides summary statistics of 2d gridded data as time series by area
#'
#' descriptions
#'
#' @data.in Either a character vector of full input file names for a list of spatRasters
#' @output.files character vector of full output file names corresponding to each input file
#' @shp.file  string. Shape file you wish to crop each input file to
#' @var.name string. Variable name you wish to extract 
#' @var.name string. Variable name you wish to extract 
#' @statistic string. Which statistic to calculate
#' @agg.time character. Time scale to calculate climatology over (days,doy, months, or years)
#' @write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return a dataframe output variable summarized by timestep for each area.names
#' 
#' @export
#' 

summary_2d_ts_nc = function(data.in,output.files,shp.file,area.names,var.name,agg.time,statistic,write.out = F){
  
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
    
    data.time = terra::time(data)
    
    if(!is.na(shp.file)){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
      which.area = which(shp.str[,which.att] %in% area.names)
      
      data.stat.area.ls = list()
      for(j in 1:length(area.names)){
        
        area.data = terra::mask(data,shp.vect[which.area[j],])
        
        area.agg = terra::tapp(area.data,fun = statistic,index =agg.time)
        area.stat = terra::global(area.agg,statistic,na.rm=T)
        
        data.stat.area.ls[[j]] = data.frame(time = terra::time(area.agg),
                                            agg.time = agg.time,
                                            ls.id = ifelse(is.character(data.in),data.in[i],i),
                                            var.name = var.name,
                                            statistic = statistic,
                                            area = area.names[j],
                                            value =area.stat[,1])
      }
      data.stat.df = dplyr::bind_rows(data.stat.area.ls)
      
    }else{
      data.agg = terra::tapp(data,fun =statistic,index = agg.time)
      data.stat = terra::global(data.agg,statistic,na.rm=T)
      
      data.stat.df = data.frame(time = terra::time(data.agg),
                                agg.time =agg.time,
                                ls.id = ifelse(is.character(data.in),data.in[i],i),
                                var.name = var.name,
                                statistic = statistic,
                                area = NA,
                                value =data.stat[,1])
    }
    
    if(write.out){
      saveRDS(data.stat.df, output.files[i])
    }else{
      out.ls[[i]] = data.stat.df
    }

  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}
