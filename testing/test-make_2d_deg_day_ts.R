# Optimized Implementation Test Script
# Load required libraries
library(terra)
library(dplyr)
library(EDABUtilities)

# Mock make_2d_summary_ts dependency to allow the standalone script to run reproducibly
make_2d_summary_ts <- function(data.in, ...) {
  list(data.frame(
    ls.id = "test_spatial_data.nc",
    time = as.Date(c('2026-01-01', '2026-01-02', '2026-01-03', '2026-01-04', '2026-01-05')),
    value = c(12.1, 16.5, 14.2, 18.1, 11.0),
    area = "test_zone_A",
    var.name = "temperature",
    agg.time = "days",
    statistic = "mean"
  ))
}

# Use standard test datasets provided by the package framework
raw_data <- terra::rast('data-raw/test_spatial_data.nc')
test_shp <- terra::vect('data-raw/test_shapefile.shp')

# Execute the optimized function
opt_ts_deg_days <- make_2d_deg_day_ts(
  data.in = raw_data,
  var.name = "temperature",
  statistic = "nd",
  ref.value = 15,
  type = "above",
  shp.file = test_shp,
  area.names = c("test_zone_A"),
  write.out = FALSE
)

# View resulting output (should safely match the conformed execution structure without breaking on empty subsets)
print(opt_ts_deg_days)
