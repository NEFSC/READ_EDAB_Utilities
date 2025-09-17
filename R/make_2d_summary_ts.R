#' Provides summary statistics of 2d gridded data as time series by area
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param file.time string. What time scale the input files are on ('daily','monthly','annual')? Assumes all monthly or annual files are on a daily timestep
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param statistics character vector. Which statistic to calculate
#' @param agg.time character. Time scale to calculate climatology over (days,doy, months,season, or  years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#'
#' @return a dataframe output variable summarized by timestep for each area.names
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_2d_summary_ts = function(data.in,file.time,output.files,shp.file,area.names,var.name,agg.time,tz = NA,statistics,touches =T,write.out = F){
  

  if(class(shp.file) %in% c('SpatVector','SpatRaster')){
    shp.vect = shp.file
    use.shp =T
  }else if(!is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
    use.shp =T
  }else{
    use.shp = F
  }
  
  out.ls = list()
  for(i in 1:length(data.in)){

    if(file.time == 'annual'){
      if(is.character(data.in)){
        
        data = terra::rast(data.in[i])
        
      }else if(class(data.in[[i]])[1] == 'SpatRaster'){
        
        if(class(data.in) == 'list'){
          data = data.in[[i]]  
        }else{
          data = data.in
        }
        
      }else{
        stop('data.in needs to be either file names or spatRasters')
      } 
      
      file.date = terra::time(data)
    }else if (file.time == 'daily'){
      
      if(is.character(data.in)){
        
        data = lapply(data.in,function(x) terra::rast(x))
        file.date = as.Date(gsub( '.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', data.in))
        
      }else if(class(data.in[[i]])[1] == 'SpatRaster'){
        
        data = lapply(data.in,function(x) terra::rast(x))
        file.date = as.Date(sapply(data,function(x) terra::time(x)))
      }else{
        stop('data.in needs to be either file names or spatRasters')
      } 
      
      data = terra::rast(data)
      terra::time(data) = file.date
      
      
    }else if(file.time == 'monthly'){
      print('monthly files not yet implemented')
    }else{
      stop('file.time must be either annual, daily, or monthly')
    }

    if(!is.na(tz)){
      file.date = as.Date(as.POSIXct(file.date,tz = tz),tz = tz)
      terra::time(data) = file.date
    }

    if(agg.time == 'season'){
      month.season = data.frame(month=1:12,season =rep(1:4,each =3))
      data.month = as.numeric(format(file.date,format = "%m"))
      data.season = month.season$season[data.month]
      season.names = 1:4
    }

    
    agg.stat.ls = list()
    for(s in 1:length(statistics)){
        if(use.shp == T){
        
        shp.str = as.data.frame(shp.vect)
        which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
        which.area =  match(area.names,shp.str[,which.att])
        
        data.stat.area.ls = list()
        for(j in 1:length(area.names)){
          
          area.data = terra::crop(terra::mask(data,shp.vect[which.area[j],], touches = touches),shp.vect[which.area[j],])
          
          if(agg.time == 'season'){
            
  
            area.agg = terra::tapp(area.data,
                                   fun = statistics[s],
                                   index =data.season)
            time.out = sort(unique(data.season))
          }else{
            area.agg = terra::tapp(area.data,
                                   fun = statistics[s],
                                   index =agg.time)  
            time.out = terra::time(area.agg)
          }
          
          area.stat = terra::global(area.agg,statistics[s],na.rm=T)
          
          data.stat.area.ls[[j]] = data.frame(time = time.out,
                                              agg.time = agg.time,
                                              ls.id = ifelse(is.character(data.in),data.in[i],i),
                                              var.name = var.name,
                                              statistic = statistics[s],
                                              area = area.names[j],
                                              value =area.stat[,1])
        }
        agg.stat.ls[[s]] = dplyr::bind_rows(data.stat.area.ls)
        
      }else{
        
        if(agg.time == 'season'){
  
          data.agg = terra::tapp(data,fun =statistics[s],index = data.season)
          time.out = sort(unique(data.season))
  
        }else{
          data.agg = terra::tapp(data,fun =statistics[s],index = agg.time)
          time.out = terra::time(data.agg)
        }
        
        data.stat = terra::global(data.agg,statistics[s],na.rm=T)
        
        agg.stat.ls[[s]] = data.frame(time = time.out,
                                  agg.time =agg.time,
                                  ls.id = ifelse(is.character(data.in),data.in[i],i),
                                  var.name = var.name,
                                  statistic = statistics[s],
                                  area = NA,
                                  value =data.stat[,1])
      }
      
    }
    data.stat.df = dplyr::bind_rows(agg.stat.ls)
    
    
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
