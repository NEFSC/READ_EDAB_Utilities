# Optimized Implementation Test Script
# Load required libraries
library(terra)
library(EDABUtilities)

# Use standard test datasets provided by the package framework
raw_data <- terra::rast('data-raw/test_spatial_data.nc')
test_shp <- terra::vect('data-raw/test_shapefile.shp')

# Execute the optimized function
opt_deg_days <- make_2d_deg_day_gridded_nc(
  data.in = raw_data,
  var.name = "temperature",
  statistic = "nd",
  ref.value = 15,
  type = "above",
  shp.file = test_shp,
  area.names = c("test_zone_A"),
  write.out = FALSE
)

# Plotting output to verify successfully generated degree-day list object matches prior results
terra::plot(opt_deg_days[[1]], main = "Optimized Number of Days > 15")