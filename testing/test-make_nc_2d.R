# Reproducible Test Script for Optimized Version
library(terra)

test_raster <- terra::rast('data-raw/test_spatial_data.nc')
test_shp    <- terra::vect('data-raw/test_shapefile.shp')

# Execute optimized function
masked_rasters_opt <- mask_nc_2d(
  data.in    = test_raster,
  var.name   = "temp",          
  min.value  = 15,               
  max.value  = 25,               
  write.out  = FALSE,
  shp.file   = test_shp,
  binary     = TRUE,             
  area.names = c("test_zone_A")  
)

# Verify outputs evaluate efficiently
print(masked_rasters_opt)
terra::plot(masked_rasters_opt[[1]], main = "Optimized Masked Output (Binary)")
