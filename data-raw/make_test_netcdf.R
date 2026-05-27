# Install terra if you haven't already: install.packages("terra")
library(terra)

# 1. Set seed to guarantee reproducibility for unit tests
set.seed(123)

# 2. Define grid dimensions and temporal span
n_lon <- 20
n_lat <- 20
n_time <- 5

# 3. Create a base raster template 
# Using a standard geographic CRS (WGS84) and arbitrary bounds
base_raster <- rast(
  nrows = n_lat, 
  ncols = n_lon, 
  xmin = -100, xmax = -80, 
  ymin = 30,   ymax = 50, 
  crs = "EPSG:4326"
)

# 4. Generate multi-layer SpatRaster (representing 2D slices over time)
# We use lapply to create a list of rasters, then combine them into one object
raster_list <- lapply(1:n_time, function(i) {
  r <- base_raster
  # Fill with pseudo-random reproducible data and add a slight time trend
  values(r) <- runif(n_lat * n_lon, min = 10, max = 25) + i 
  return(r)
})

r_stack <- rast(raster_list)

# 5. Assign the time dimension
# terra will automatically translate this to a time coordinate variable in the netCDF
time_steps <- as.Date("2026-01-01") + 0:(n_time - 1)
time(r_stack) <- time_steps

# 6. Write out the netCDF file
output_file <- here::here('data-raw',"test_spatial_data.nc")

# writeCDF is terra's dedicated function for netCDF export with full metadata control
writeCDF(
  r_stack, 
  filename = output_file, 
  varname = "test_var", 
  longname = "Simulated 2D Test Variable", 
  unit = "arbitrary_units", 
  overwrite = TRUE
)

# Optional: Print summary to verify the structure in the console
cat("NetCDF file generated successfully!\n")
print(r_stack)

### make Shp file
# 1. Define the coordinates for the polygon
# Ensuring these coordinates fall completely within the raster bounds 
# (Raster bounds were: xmin = -100, xmax = -80, ymin = 30, ymax = 50)
poly_coords <- matrix(c(
  -95, 35,  # Bottom-left
  -95, 45,  # Top-left
  -85, 45,  # Top-right
  -85, 35,  # Bottom-right
  -95, 35   # Close the polygon (same as first point)
), ncol = 2, byrow = TRUE)

# 2. Convert the coordinate matrix into a terra SpatVector polygon
test_polygon <- vect(poly_coords, type = "polygons", crs = "EPSG:4326")

# 3. Add an attribute table (data.frame) to the polygon
# This is highly recommended for unit testing spatial extraction/joins
values(test_polygon) <- data.frame(
  id = 1,
  zone_name = "test_zone_A",
  weight_factor = 1.5
)

# 4. Write out the shapefile
output_shp <- here::here('data-raw',"test_shapefile.shp")

# writeVector generates the .shp, .shx, .dbf, and .prj files needed
writeVector(
  test_polygon, 
  filename = output_shp, 
  overwrite = TRUE
)

# Optional: Print summary to verify the structure in the console
cat("Shapefile generated successfully!\n")
print(test_polygon)
