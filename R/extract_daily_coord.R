#' Matches daily coordinates to gridded data
#'
#' descriptions
#'
#' @param input.dir String. The full input directory for daily input files
#' @param input.prefix string. The prefix of the input files
#' @param input.type string. The type of input data (e.g., 'daily', 'annual'), assumes data within at daily scale
#' @param output.dir string. The full output directory for the gridded data
#' @param output.prefix string. The prefix of the output files
#' @param coordinates dataframe of lat, lon, and date to be extracted from daily data
#' @param search.radius numeric. The number of cell "rings" around the closest match to aggregate over. 0 = closest cell, 1 = 3x3 cells around closest, etc
#' @param statistics character vector. The statistics to be used for the gridded data. Options are 'mean', 'median', 'min', 'max', 'sd', 'var', 'sum'
#' @param var.name string. The name of the variable being extracted, used for output file naming
#' @param write.out logical. If TRUE, writes the output to a csv file, if FALSE returns the output as a dataframe
#' 
#' @return a csv with appended values from coordinates input
#' 
#' @export
#' 

# input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/'
# input.dir = 'C:/Data/GLORYS/Daily_Bottom_Temp/2022/'
# # input.prefix = 'GLORYS_daily_BottomTemp'
# input.prefix = 'GLORYS_REANALYSIS_'
# input.type = 'daily'
# output.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/bts_stations/'
# output.prefix = 'bottom_trawl_survey_stations_GLORYS_2022_'
# coordinates = readRDS(here::here('data-raw','station_locations.rds')) %>% rename(lat = 'LAT',lon = 'LON',date = 'EST_TOWDATE')
# var.name = 'theao'


extract_daily_coord = function(input.dir, input.prefix,input.type, output.dir, output.prefix, coordinates, search.radius = 0,var.name, statistics, write.out){
  
  #List input files
  input.files.short = list.files(input.dir, pattern = paste0(input.prefix, '.*\\.nc'), full.names = F)
  if(input.type == 'daily'){
    input.file.date = as.Date(gsub( '.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', input.files.short))  
    input.file.year = format(input.file.date,format = '%Y')
  }else{
    
    #get 4digit year from input.files.short
    input.file.year = gsub( '.*_([0-9]{4}).*', '\\1', input.files.short)
  }
  
  
  #Check if output file exists
  output.file = paste0(output.dir, output.prefix, coordinates$lat[1], '_', coordinates$lon[1], '.csv')
  
  if(file.exists(output.file)){
    message(paste0('Output file already exists: ', output.file))
    return(NULL)
  }
  
  #Create output directory if it doesn't exist
  if(!dir.exists(output.dir)){
    dir.create(output.dir, recursive = TRUE)
  }
  
  #Setup output.df
  coordinates$date = as.Date(coordinates$date)
  
  output.ls = list()
  
  if(input.type == 'daily'){
    #match dates in input.files to coodinates
    # coord.dates = sort(unique(as.Date(output.df$date)))
    input.date.match = input.file.date[which(input.file.date %in% coordinates$date)]
    
    output.ls = list()
    
    for(i in 1:length(input.date.match)){
      
      #Get the date for this file
      this.date = input.date.match[i]
      
      #Get the file name for this date
      this.file = paste0(input.dir,input.files.short[which(input.file.date == this.date)])
      
      #Read in netCDF
      this.data = terra::rast(this.file,subds = var.name)
      
      #which output.df match this.date
      which.coord.date = which(as.character(coordinates$date) == this.date)
      this.coords = coordinates[which.coord.date,] %>%
        dplyr::select(lon,lat)%>%
        as.matrix()
      
      #Extract from raster
      this.coords.vals = terra::extract(this.data,this.coords,cells = T)
      val.product.coords =  terra::xyFromCell(this.data,this.coords.vals$cell)
      
      out.match = data.frame(lon.obs = this.coords[,1],
                             lat.obs = this.coords[,2],
                             lon.product = val.product.coords[,1],
                             lat.product = val.product.coords[,2],
                             center.value = this.coords.vals[,2],
                             date =this.date,
                             var.name = var.name,
                             search.radius = search.radius,
                             center.cell = this.coords.vals$cell,
                             stringsAsFactors = F)
      output.ls[[i]] = out.match
      
      
      
      ##should return a list of dataframes##
      if(search.radius > 0){
        
        #extract a in a ring around a coordinate match
        val.product.rc = terra::rowColFromCell(this.data,this.coords.vals$cell) %>%
          as.data.frame() %>%
          dplyr::rename(x.center = 'V1',y.center = 'V2')%>%
          dplyr::mutate(center.cell = this.coords.vals$cell,
                        x.min = x.center - search.radius,
                        x.max = x.center + search.radius,
                        y.min = y.center - search.radius,
                        y.max = y.center + search.radius)
        
        output.stat.ls = list()
        for(k in 1:nrow(val.product.rc)){
          #get the row and column for this coordinate
          this.row = val.product.rc$x.center[k]
          this.col = val.product.rc$y.center[k]
          
          #get the rows and columns for the box
          this.row.range = (this.row - search.radius):(this.row + search.radius)
          this.col.range = (this.col - search.radius):(this.col + search.radius)
          
          #get the cells in the box
          this.box.cells = terra::cellFromRowCol(this.data,rep(this.row.range, length(this.col.range)),rep(this.col.range,each = length(this.col.range)))
          
          #extract from raster
          this.box.vals = terra::extract(this.date.rast,this.box.cells)[,1]
          
          #get the summary statistics for this box
          output.stat.ls[[k]] = lapply(statistics, function(stat){
            if(stat == 'mean'){
              this.box.stat = mean(this.box.vals, na.rm = T)
            }else if(stat == 'median'){
              this.box.stat = median(this.box.vals, na.rm = T)
            }else if(stat == 'min'){
              this.box.stat = min(this.box.vals, na.rm = T)
            }else if(stat == 'max'){
              this.box.stat = max(this.box.vals, na.rm = T)
            }else if(stat == 'sd'){
              this.box.stat = sd(this.box.vals, na.rm = T)
            }else if(stat == 'var'){
              this.box.stat = var(this.box.vals, na.rm = T)
            }else if(stat == 'sum'){
              this.box.stat = sum(this.box.vals, na.rm = T)
            }
            stat.out = val.product.rc[k,] %>%
              dplyr::mutate(statistic = stat,value = this.box.stat)
            return(stat.out)
          })%>%
            dplyr::bind_rows()
          
        }
        output.stat.df = dplyr::bind_rows(output.stat.ls) %>%
          left_join(out.match)
        
        output.ls[[i]] = output.stat.df
      }
      
    }
  }else if(input.type == 'annual'){
    
    coord.years = format(coordinates$date,format = '%Y')
    
    #match input.file years to coord.years
    input.file.year.match = input.file.year[which(input.file.year %in% coord.years)]
    
    ind = 1
    
    for(i in 1:length(input.file.year.match)){
      
      #Get the year for this file
      this.year = input.file.year.match[i]
      
      #Get the file name for this year
      this.file = paste0(input.dir,input.files.short[which(input.file.year == this.year)])
      
      #Read in netCDF
      this.data = terra::rast(this.file,subds = var.name)
      this.data.time = as.character(terra::time(this.data))
      
      #Match this dates from this.year to this.data
      coord.year.dates = sort(unique(as.Date(coordinates$date[which(format(coordinates$date,format = '%Y') == this.year)])))
      coord.year.dates = coord.year.dates[which(coord.year.dates %in% this.data.time)]
      
      for(j in 1:length(coord.year.dates)){
        
        this.date = which(this.data.time == coord.year.dates[j])
        which.coord.date = which(as.character(coordinates$date) == coord.year.dates[j])
        
        #get coordinates for this date
        this.coords = dplyr::filter(coordinates, date == coord.year.dates[j]) %>%
          dplyr::select(lon,lat) %>%
          as.matrix()
        
        this.date.rast = terra::subset(this.data,this.date)
        
        #Extract from raster
        this.coords.vals = terra::extract(this.date.rast,this.coords,cells = T)
        val.product.coords =  terra::xyFromCell(this.data,this.coords.vals$cell)
        
        ## change to index
        out.match = data.frame(lon.obs = this.coords[,1],
                               lat.obs = this.coords[,2],
                               lon.product = val.product.coords[,1],
                               lat.product = val.product.coords[,2],
                               center.value = this.coords.vals[,2],
                               date = as.Date(coord.year.dates[j]),
                               var.name = var.name,
                               search.radius = search.radius,
                               center.cell = this.coords.vals$cell,
                               stringsAsFactors = F)
        output.ls[[ind]] = out.match
        
        
        #Do search radius
        #extract a in a ring around a coordinate match
        val.product.rc = terra::rowColFromCell(this.data,this.coords.vals$cell) %>%
          as.data.frame() %>%
          dplyr::rename(x.center = 'V1',y.center = 'V2')%>%
          dplyr::mutate(center.cell = this.coords.vals$cell,
                        x.min = x.center - search.radius,
                        x.max = x.center + search.radius,
                        y.min = y.center - search.radius,
                        y.max = y.center + search.radius)
        
        #loop through val.product.rc and extract box defined by x1,x2,y1,y2
        
        
        ##should return a list of dataframes##
        if(search.radius > 0){
          output.stat.ls = list()
          for(k in 1:nrow(val.product.rc)){
            #get the row and column for this coordinate
            this.row = val.product.rc$x.center[k]
            this.col = val.product.rc$y.center[k]
            
            #get the rows and columns for the box
            this.row.range = (this.row - search.radius):(this.row + search.radius)
            this.col.range = (this.col - search.radius):(this.col + search.radius)
            
            #get the cells in the box
            this.box.cells = terra::cellFromRowCol(this.data,rep(this.row.range, length(this.col.range)),rep(this.col.range,each = length(this.col.range)))
            
            #extract from raster
            this.box.vals = terra::extract(this.date.rast,this.box.cells)[,1]
            
            #get the summary statistics for this box
            output.stat.ls[[k]] = lapply(statistics, function(stat){
              if(stat == 'mean'){
                this.box.stat = mean(this.box.vals, na.rm = T)
              }else if(stat == 'median'){
                this.box.stat = median(this.box.vals, na.rm = T)
              }else if(stat == 'min'){
                this.box.stat = min(this.box.vals, na.rm = T)
              }else if(stat == 'max'){
                this.box.stat = max(this.box.vals, na.rm = T)
              }else if(stat == 'sd'){
                this.box.stat = sd(this.box.vals, na.rm = T)
              }else if(stat == 'var'){
                this.box.stat = var(this.box.vals, na.rm = T)
              }else if(stat == 'sum'){
                this.box.stat = sum(this.box.vals, na.rm = T)
              }
              stat.out = val.product.rc[k,] %>%
                dplyr::mutate(statistic = stat,value = this.box.stat)
              return(stat.out)
            })%>%
              dplyr::bind_rows()
            
          }
          output.stat.df = dplyr::bind_rows(output.stat.ls) %>%
            left_join(out.match)
          
          output.ls[[ind]] = output.stat.df
        }
        
        ind = ind +1
      }
    }
    
  }else{
    stop('Input type not recognized. Please use daily or annual.')
  }
  
  output.df = dplyr::bind_rows(output.ls)
    # filter(!is.na(value))
  
  if(write.out){
    write.csv(output.df,paste0(output.dir,output.prefix,'_',var.name,'.csv'),row.names =F)
  }else{
    return(output.df)
  }
}