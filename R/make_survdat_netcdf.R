# Load the ncdf4 package
library(ncdf4)
library(dplyr)

data = read.csv(here::here('data','survdat_sweptarea_all.csv'))

var.index = data %>%
  dplyr::select(variable,units)%>%
  dplyr::distinct()

# var.names = unique(data$variable)
years = sort(unique(data$YEAR))
spp.codes = sort(unique(data$SVSPP)) #Would want spp names not just svspp

# Define dimensions
time_dim <- ncdim_def(name = "year", units = "years", vals = years)
spp_dim <- ncdim_def(name = "SVSPP", units = "", vals = spp.codes)

# Build template for variable values
var.blank = matrix(data = -999, nrow = length(years), ncol = length(spp.codes))

# Define variables in a loop
var.ls = list()
for(i in 1:nrow(var.index)){
  
  #fill in long name after reading documentation
  var.ls[[i]] <- ncvar_def(name = var.index$variable[i], units = var.index$units[i], dim = list(time_dim,spp_dim), 
                        missval = -999, longname = var.index$variable[i], prec = "float")
  
}

# Create NetCDF file
nc_file <- nc_create(here::here('data',"survdat_example.nc"), vars = var.ls)

# Add global attributes
ncatt_put(nc_file, 0, "title", "Example Survdat Pull")

# Puts all the values in 
for(j in 1:nrow(var.index)){
  
  var.data = data %>%
    filter(variable == var.index$variable[j]) %>%
    select(YEAR,SVSPP,value)
  
  #turn var.data into a matrix with the structure of var.blank
  var.wide =var.data %>%
    tidyr::spread(key = SVSPP, value = value)%>%
    select(-YEAR)%>%
    as.matrix()
  
  var.wide[which(is.na(var.wide))] = -999
 
  # Write data to file
  ncvar_put(nc_file, var.ls[[j]], var.wide)

  # Add attributes to the variable
  ncatt_put(nc_file, var.ls[[j]], "Region", 'NEUS LME')
  
}

# Close the NetCDF file
nc_close(nc_file)

# Check the contents of the NetCDF file

nc_file_check <- nc_open(here::here('data',"survdat_example.nc"))

ncvar_get(nc_file_check, var.ls[[1]]) %>% View()

ncatt_get(nc_file,0)

ncatt_get(nc_file,var.ls[[1]])
