# -------------------------------------------------------------------------
# Test script for Optimized extract_daily_coord
# -------------------------------------------------------------------------
library(terra)
library(dplyr)

# 1. Load Standard Test Data and set time attributes explicitly
test_raster <- terra::rast("data-raw/test_spatial_data.nc")
terra::time(test_raster) <- as.Date("2026-01-01") + 0:4

# 2. Generate mock pipeline output timeseries (coordinates matching the dates above)
mock_coord_df <- data.frame(
  lat = c(40.5, 41.0, 42.0),
  lon = c(-90.5, -88.0, -85.0),
  date = as.Date(c('2026-01-02', '2026-01-04', '2026-01-08')) # Last date won't match to test filtering
)

# 3. Execute Optimized Function
result_df <- extract_daily_coord(
  data.in = test_raster,
  coord.df = mock_coord_df,
  var.name = "BottomS",
  search.radius = 1,
  statistics = c("mean", "max"),
  write.out = FALSE
)

# 4. Output Validation
print(head(result_df))
