#' Provides summary statistics of 2d gridded data as time series by area
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param statistic string. Which statistic to calculate
#' @param agg.time character. Time scale to calculate climatology over (days,doy, months,season, or  years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return a dataframe output variable summarized by timestep for each area.names
#' 
#' @export
#' 

make_2d_summary_ts = function(data.in,output.files,shp.file,area.names,var.name,agg.time,tz = NA,statistic,touches =T,write.out = F){
  
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
    
    data.time = as.Date(terra::time(data))
    if(!is.na(tz)){
      data.time = as.Date(as.POSIXct(data.time,tz = tz),tz = tz)
      terra::time(data) = data.time
    }
    
    if(agg.time == 'season'){
      month.season = data.frame(month=1:12,season =rep(1:4,each =3))
      data.month = as.numeric(format(data.time,format = "%m"))
      data.season = month.season$season[data.month]
      season.names = 1:4
    }
    
    
    if(!is.na(shp.file)){
      
      shp.str = as.data.frame(shp.vect)
      which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
      which.area =  match(area.names,shp.str[,which.att])
      
      data.stat.area.ls = list()
      for(j in 1:length(area.names)){
        
        area.data = terra::mask(data,shp.vect[which.area[j],], touches = touches)
        
        if(agg.time == 'season'){
          

          area.agg = terra::tapp(area.data,
                                 fun = statistic,
                                 index =data.season)
          time.out = season.names
        }else{
          area.agg = terra::tapp(area.data,
                                 fun = statistic,
                                 index =agg.time)  
          time.out = terra::time(area.agg)
        }
        
        area.stat = terra::global(area.agg,statistic,na.rm=T)
        
        data.stat.area.ls[[j]] = data.frame(time = time.out,
                                            agg.time = agg.time,
                                            ls.id = ifelse(is.character(data.in),data.in[i],i),
                                            var.name = var.name,
                                            statistic = statistic,
                                            area = area.names[j],
                                            value =area.stat[,1])
      }
      data.stat.df = dplyr::bind_rows(data.stat.area.ls)
      
    }else{
      
      if(agg.time == 'season'){

        data.agg = terra::tapp(data,fun =statistic,index = data.season)
        time.out = season.names

      }else{
        data.agg = terra::tapp(data,fun =statistic,index = agg.time)
        time.out = terra::time(data.agg)
      }
      
      data.stat = terra::global(data.agg,statistic,na.rm=T)
      
      data.stat.df = data.frame(time = time.out,
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
