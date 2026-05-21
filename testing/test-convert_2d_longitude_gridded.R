# Load terra package and standard testing data
library(terra)

# Execute identical test utilizing the optimized function mapping
test_raster <- terra::rast('data-raw/test_spatial_data.nc')

# The resulting list matches exactly to the conformed script test
optimized_result_ls <- convert_2d_longitude_gridded(data.in = test_raster, write.out = FALSE)

# Verify identity and integrity of output
print(names(optimized_result_ls))
print(terra::ext(optimized_result_ls[[1]]))