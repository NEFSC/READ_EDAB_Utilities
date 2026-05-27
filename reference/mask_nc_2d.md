# Masks a 2D netCDF using a lower and upper value

This function filters 2D gridded spatial data by clamping values within
a specified minimum and maximum range. It can optionally convert the
valid range into a binary mask and crop the data to a specific spatial
area prior to masking.

## Usage

``` r
mask_nc_2d(
  data.in,
  var.name,
  min.value,
  max.value,
  write.out = FALSE,
  output.files = NULL,
  shp.file = NA,
  binary = FALSE,
  area.names = NA
)
```

## Arguments

- data.in:

  character vector, list, or SpatRaster. Single file path, vector of
  file paths, single SpatRaster, or list of SpatRasters.

- var.name:

  character. Variable name you wish to extract and process.

- min.value:

  numeric. Minimum value of the variable to retain.

- max.value:

  numeric. Maximum value of the variable to retain.

- write.out:

  logical. If TRUE, writes a netCDF file. If FALSE, returns a list of
  SpatRasters. Default is FALSE.

- output.files:

  character vector. Full output file names corresponding to each input
  item. Required if write.out is TRUE.

- shp.file:

  string, SpatVector, or SpatRaster. Shapefile to crop and mask each
  input file to. Default is NA.

- binary:

  logical. Whether the mask should be binary (1 for inside range, NA for
  outside) or retain original values. Default is FALSE.

- area.names:

  character vector. Specific area names to filter the shapefile by
  before cropping. Default is NA.

## Value

A named list of SpatRasters if write.out is FALSE. Otherwise, writes
NetCDF files and returns nothing.
