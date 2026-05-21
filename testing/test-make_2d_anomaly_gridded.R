# -------------------------------------------------------------------------
# Test script for Optimized make_2d_anomaly_gridded
# -------------------------------------------------------------------------
library(terra)

# Mocking the EDABUtilities environment for standard pipeline dependencies
# This ensures EDABUtilities::convert_longitude() resolves during testing
if (!exists("EDABUtilities")) {
  EDABUtilities <- new.env()
  EDABUtilities$convert_longitude <- function(r) { return(r) }
}

# 1. Load Standard Test Data 
# (Assumes working directory contains the package's 'data-raw' folder)
test_raster <- terra::rast("data-raw/test_spatial_data.nc")
test_shape  <- terra::vect("data-raw/test_shapefile.shp")

# 2. Generate a mock climatology 
# For testing purposes, we use the mean of the test raster over time
mock_climatology <- terra::app(test_raster, "mean")*0+10

# 3. Execute Optimized Function
# We test masking by 'test_zone_A' and returning the raster list in-memory
result_ls <- make_2d_anomaly_gridded(
  data.in = test_raster,
  climatology = mock_climatology,
  var.name = "BottomS",
  shp.file = test_shape,
  area.names = c("test_zone_A"),
  write.out = FALSE
)

# 4. Output Validation
print(result_ls)

# Plot the first layer of the resulting anomaly to visually verify 
# the spatial masking and subtraction logic
if (length(result_ls) > 0) {
  terra::plot(test_raster)
  terra::plot(mock_climatology)
  terra::plot(result_ls[[1]], main = "Optimized Anomaly Output (test_zone_A)")
}
