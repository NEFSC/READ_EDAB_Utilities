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

agg.time = 'years'
statistic = 'mean'
area.names = c('GOM','SS')
