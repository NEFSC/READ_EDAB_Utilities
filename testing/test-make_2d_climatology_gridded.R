# Optimized Implementation Test Script
# Load required libraries
library(terra)
library(EDABUtilities)

# Use standard test datasets provided by the package framework
raw_data <- terra::rast('data-raw/test_spatial_data.nc')
test_shp <- terra::vect('data-raw/test_shapefile.shp')

# Execute the optimized function
opt_climatology <- make_2d_climatology_gridded(
  data.in = raw_data,
  var.name = "temperature",
  agg.time = "days",
  statistic = "mean",
  start.time = as.Date("2026-01-01"), 
  stop.time = as.Date("2026-01-05"),
  shp.file = test_shp,
  area.names = c("test_zone_A"),
  write.out = FALSE
)

# Plotting output to verify successfully generated climatology list object matches the prior result
terra::plot(opt_climatology[[1]], main = "Optimized Gridded Climatology")