# stephanie test script

# library(dplyr)
# pak::pak("NEFSC/READ_EDAB_Utilities@dev")
library(EDABUtilities)

library("EDABUtilities")

convert_longitude(data = here::here('data/sst_test.nc'))
test <- convert_longitude(data = here::here('data/GLORYS_daily_BottomSalinity_2019.nc'))


test_glorys <- make_2d_summary_ts(data.in = here::here('data','GLORYS_daily_BottomSalinity_2019.nc'),
                           output.files = here::here('data','GLORYS_BottomS.nc'),
                           file.time = 'monthly',
                           shp.file = here::here('data','EPU_NOESTUARIES.shp'),
                           var.name = 'BottomS',
                           agg.time ='months',
                           statistics = 'mean',
                           area.names = c('MAB','GB'),
                           tz = NA,
                           touches = TRUE,
                           write.out = F)

test_sst <- make_2d_summary_ts(data.in = here::here('data','sst_test.nc'),
                               output.files = here::here('data','sst.nc'),
                               file.time = 'monthly',
                               shp.file = here::here('data','EPU_NOESTUARIES.shp'),
                               var.name = 'sst',
                               agg.time ='months',
                               statistics = 'mean',
                               area.names = c('MAB','GB'),
                               tz = NA,
                               touches = TRUE,
                               write.out = F)

#Error for both inputs:
#Error in h(simpleError(msg, call)) : 
 # error in evaluating the argument 'x' in selecting a method for function 'crop': unable to find an inherited method for function ‘mask’ for signature ‘x = "function", mask = "SpatVector"’

# works
test_epu <- crop_nc_2d(data.in = here::here('data','GLORYS_daily_BottomSalinity_2019.nc'),
                       output.files = here::here('data','GLORYS_daily_BottomSalinity_EPU_2019.nc'),
                       shp.file = here::here('data','EPU_NOESTUARIES.shp'),
                       var.name = 'BottomS',
                       write.out = F)

terra::plot(test_epu[[1]])
# Error in xy.coords(x, y, xlabel, ylabel, log) : 
#   'x' is a list, but does not have components 'x' and 'y'

img <- terra::plot(test_epu[[1]])
img
# returns NULL