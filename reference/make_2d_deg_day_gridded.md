# Provides a gridded summary of degree-day family statistics

This function aggregates temporal spatial data to calculate degree-day
statistics based on a reference threshold. It can optionally mask the
input to specific shapefile regions before processing.

## Usage

``` r
make_2d_deg_day_gridded(
  data.in,
  var.name,
  statistic,
  ref.value,
  type,
  shp.file = NA,
  area.names = NULL,
  output.files = NULL,
  write.out = FALSE
)
```

## Arguments

- data.in:

  character vector, list, or SpatRaster. Single file path, vector of
  file paths, single SpatRaster, or list of SpatRasters representing the
  data to be processed.

- var.name:

  character. Variable name you wish to extract and process.

- statistic:

  character. Which statistic to calculate ('dd' for degree days, 'nd'
  for number of days, 'nd.con' for max consecutive number of days).

- ref.value:

  numeric. Reference point value for the threshold.

- type:

  character. How to use the reference point ('above', 'below', or
  'raw').

- shp.file:

  character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask
  the input data to. Default is NA.

- area.names:

  character vector or NULL. Names of shapefile areas you want to retain.
  Default is NULL.

- output.files:

  character vector or NULL. Full output file paths corresponding to each
  input file if write.out is TRUE. Default is NULL.

- write.out:

  logical. If TRUE, writes a netCDF file. If FALSE, returns a named list
  containing the SpatRasters. Default is FALSE.

## Value

If write.out is TRUE, writes a NetCDF file with the same spatial
dimensions as the input file. If FALSE, returns a named list of
SpatRasters.
