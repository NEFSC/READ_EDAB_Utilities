#' Provides a data frame of degree-day family statistics
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatrasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param statistic string. Which statistic to calculate ('dd' for degree days, 'nd' for number of days)
#' @param ref.value numeric. reference point value for threshold
#' @param type string. How to use reference point ('above', 'below', or 'raw')
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return a dataframe output variable summarized by timestep for each area.names
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export


make_2d_deg_day_ts = function(data.in,output.files,shp.file,area.names,var.name,statistic,ref.value,type,write.out = F){
  
  if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
  }
  
  data.summary = make_2d_summary_ts(data.in = data.in,
                                    write.out = F,
                                    shp.file = shp.file,
                                    var.name = var.name,
                                    agg.time = 'days',
                                    statistic = 'mean',
                                    area.names =area.names)
  
  out.ls = list()
  nd.con.fun = function(x){
    l = rle(x)
    m = l$lengths[which(l$values == 1)]
    
    return(ifelse(length(m) == 0, 0,max(m,na.rm=T)))
  }
  
  for(i in 1:length(data.summary)){
    
    if(type == 'raw'){
      
      data.stat = data.summary[[i]] %>%
        dplyr::group_by(ls.id,var.name,statistic,area)%>%
        dplyr::summarise(value = sum(value,na.rm=T))%>%
        dplyr::mutate(statistic = statistic)
      
    }else if (type == 'above'){
      
      if(statistic == 'dd'){
        
        data.stat =  data.summary[[i]]  %>%
          dplyr::filter(value > ref.value)%>%
          dplyr::group_by(ls.id,var.name,statistic,area)%>%
          dplyr::summarise(value = sum(value,na.rm=T))%>%
          dplyr::mutate(statistic = statistic)

      }else if (statistic == 'nd'){
        
        data.stat =  data.summary[[i]]  %>%
          dplyr::filter(value > ref.value)%>%
          dplyr::group_by(ls.id,var.name,statistic,area)%>%
          dplyr::summarise(value = dplyr::n())%>%
          dplyr::mutate(statistic = statistic)
        
      }else if(statistic == 'nd.con'){
        

        data.stat =  data.summary[[i]]  %>%
          dplyr::mutate(value.flag = value > ref.value)%>%
          dplyr::group_by(agg.time,var.name,statistic,area)%>%
          dplyr::summarise(statistic = nd.con.fun(value.flag))
          
       }else{
        
        warning('statistic needs to be "dd" or "nd"')
      }
      
    }else if (type == 'below'){
      
      if(statistic == 'dd'){
        
        data.stat =  data.summary[[i]]  %>%
          dplyr::filter(value < ref.value)%>%
          dplyr::group_by(ls.id,var.name,statistic,area)%>%
          dplyr::summarise(value = sum(value,na.rm=T))%>%
          dplyr::mutate(statistic = statistic)
        
      }else if (statistic == 'nd'){
        
        data.stat =  data.summary[[i]]  %>%
          dplyr::filter(value < ref.value)%>%
          dplyr::group_by(ls.id,var.name,statistic,area)%>%
          dplyr::summarise(value = n())%>%
          dplyr::mutate(statistic = statistic)
        
        
      }else if(statistic == 'nd.con'){
        data.stat =  data.summary[[i]]  %>%
          dplyr::mutate(value.flag = value < ref.value)%>%
          dplyr::group_by(agg.time,var.name,statistic,area)%>%
          dplyr::summarise(statistic = nd.con.fun(value.flag))
      }else{
        warning('statistic needs to be "dd" or "nd"')
      }
    }
      
    if(write.out){
      saveRDS(data.stat.df, output.files[i])
    }else{
      out.ls[[i]] = data.stat
    }
    
  }
  
  if(write.out ==F){
    return(out.ls)  
  }
  
}
