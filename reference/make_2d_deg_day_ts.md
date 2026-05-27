# Calculates timeseries degree-day family statistics from spatial data

This function calculates degree-day family statistics (like number of
days or consecutive days above/below a threshold) by first summarizing
spatial input data into a daily timeseries, then evaluating the
timeseries against a reference value.

## Usage

``` r
make_2d_deg_day_ts(
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
  spatial data to be processed.

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

  logical. If TRUE, writes RDS files. If FALSE, returns a list of
  data.frames. Default is FALSE.

## Value

If write.out is TRUE, writes RDS files to disk. If FALSE, returns a list
of data.frames summarized by timestep and area.
