library(dplyr)
library(ggplot2)
library(terra)
data.in = c(here::here('test_data','GLORYS_daily_BottomSalinity_2019.nc'),
                here::here('test_data','GLORYS_daily_BottomSalinity_2020.nc'))
output.files = c(here::here('test_data','GLORYS_daily_BottomSalinity_EPU_mask_2019.nc'),
                here::here('test_data','GLORYS_daily_BottomSalinity_EPU_mask_2020.nc'))
shp.file = here::here('test_data','EPU_NOESTUARIES.shp')
source(here::here('R','crop_nc_2d.R'))
source(here::here('R','mask_nc_2d.R'))
var.name = 'BottomS'      

data.in = crop_nc_2d(
  input.files = c(here::here('test_data','GLORYS_daily_BottomSalinity_2019.nc'),
                  here::here('test_data','GLORYS_daily_BottomSalinity_2020.nc')),
  output.files = c(here::here('test_data','GLORYS_daily_BottomSalinity_EPU_2019.nc'),
                   here::here('test_data','GLORYS_daily_BottomSalinity_EPU_2020.nc')),
  shp.file = here::here('test_data','EPU_NOESTUARIES.shp'),
  var.name = 'BottomS',
  write.out = F

)


min.value = 30
max.value = 50



data.mask = mask_nc_2d(
  data.in = data.in,
  shp.file = shp.file,
  min.value = 30,
  max.value = 33,
  binary = F
)
plot(data.mask[[1]][[1]])

agg.time = 'months'
statistic = 'mean'
area.names = c('GOM','SS','MAB')
binary =T

source(here::here('R','summary_2d_gridded_nc.R'))
grid.test =summary_2d_gridded_nc(data.in = data.in[1],
                      write.out = F,
                      shp.file = shp.file,
                      var.name = var.name,
                      agg.time = agg.time,
                      statistic = statistic,
                      area.names = area.names)
climatology = grid.test[[1]]
plot(grid.test[[1]])  
plot(grid.test[[2]])  
  
source(here::here('R','summary_2d_ts_nc.R'))
test.ts = summary_2d_ts_nc(data.in = data.in,
                           shp.file = shp.file,
                           var.name = var.name,
                           agg.time =agg.time,
                           statistic = statistic,
                           area.names = area.names,
                           write.out = F)

ts.test.df = dplyr::bind_rows(test.ts)
ggplot(ts.test.df, aes(x = time, y = value))+
  geom_line()+
  facet_wrap(~area)

data.in = test.ts

start.time = 1
stop.time = 4

source(here::here('R','make_2d_climatology_gridded.R'))
climatology = make_2d_climatology_gridded(data.in = data.in,
                                          write.out = F,
                                          output.files,
                                          shp.file,
                                          var.name,
                                          area.names,
                                          start.time, 
                                          stop.time,
                                          agg.time,
                                          statistic
                                          )

source(here::here('R','make_2d_anomaly_gridded.R'))
anom.test =make_2d_anomaly_gridded(data.in = data.in,
                                   climatology,
                                   output.files,
                                   shp.file,
                                   var.name,
                                   area.names = area.names,
                                   write.out =F)
plot(anom.test[[1]])

source(here::here('R','make_2d_anomaly_ts.R'))
anom.ts.test = make_2d_anomaly_ts(data.in = test.ts,
                   climatology = test.ts[[1]],
                   output.files,
                   write.out = F)
