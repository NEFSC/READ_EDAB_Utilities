# -------------------------------------------------------------------------
# Reproducible Test script
# -------------------------------------------------------------------------
library(terra)

# Mocking the EDABUtilities environment for standard pipeline function dependencies
if (!exists("EDABUtilities")) {
  EDABUtilities <- new.env()
  EDABUtilities$convert_longitude <- function(r) { return(r) }
}

# 1. Load Standard Test Data (assuming working dir has data-raw folder)
test_raster <- terra::rast("data-raw/test_spatial_data.nc")
test_shape  <- terra::vect("data-raw/test_shapefile.shp")

# 2. Execute Optimized Function
result_ls <- crop_nc_2d(
  data.in = test_raster,
  shp.file = test_shape,
  var.name = "BottomS",
  area.names = c("test_zone_A"), 
  write.out = FALSE
)

# 3. Output Validation
print(result_ls)
if (length(result_ls) > 0) terra::plot(result_ls[[1]], main = "Optimized Cropped Output")