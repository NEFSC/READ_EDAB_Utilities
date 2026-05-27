# Convert spatial object longitude to standard format

This function converts the longitudinal coordinates of spatial objects
from a 0-360 degree scale to a standard -180 to +180 degree scale. It
accepts raw spatial objects or file paths and processes them into a
standard list.

## Usage

``` r
convert_2d_longitude_gridded(data.in, write.out = FALSE, output.files = NA)
```

## Arguments

- data.in:

  character vector, list, or SpatRaster. Single file path, vector of
  file paths, single SpatRaster, or list of SpatRasters/SpatVectors.

- write.out:

  logical. If TRUE, writes a netCDF file (requires output.files). If
  FALSE, returns a list of spatial objects. Default is FALSE.

- output.files:

  character vector. Paths for output files if write.out is TRUE. Default
  is NA.

## Value

A named list of SpatRaster or SpatVector objects with standardized
longitudes.
