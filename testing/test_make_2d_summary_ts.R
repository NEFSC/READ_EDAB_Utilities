# Optimized Implementation Test Script
# Load required libraries
library(terra)
library(dplyr)
library(EDABUtilities)

# Use standard test datasets provided by the package framework
raw_data <- terra::rast('data-raw/test_spatial_data.nc')
test_shp <- terra::vect('data-raw/test_shapefile.shp')

# Execute the optimized function
opt_summary_ts <- make_2d_summary_ts(
  data.in = raw_data,
  var.name = "temperature",
  statistics = c("mean", "max"),
  agg.time = "months",
  file.time = "annual",
  shp.file = test_shp,
  area.names = c("test_zone_A"),
  write.out = FALSE
)

# View resulting output (should perfectly match the conformed execution)
print(opt_summary_ts[[1]])