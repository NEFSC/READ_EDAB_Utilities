# Matches daily coordinates to gridded data

This function extracts values from a daily or multi-layer gridded
dataset at specific coordinate and time locations. It standardizes
inputs and optionally computes focal statistics within a specified
search radius around the target coordinates.

## Usage

``` r
extract_daily_coord(
  data.in,
  coord.df,
  var.name,
  search.radius = 0,
  statistics = "mean",
  write.out = FALSE,
  output.file = NULL
)
```

## Arguments

- data.in:

  character vector, list, or SpatRaster. Single file path, vector of
  file paths, single SpatRaster, or list of SpatRasters.

- coord.df:

  data.frame. A dataframe containing 'lat', 'lon', and 'date' columns to
  be extracted from the gridded data.

- var.name:

  string. The name of the variable being extracted.

- search.radius:

  numeric. The number of cell "rings" around the closest match to
  aggregate over. 0 = closest cell, 1 = 3x3 cells around closest, etc.

- statistics:

  character vector. The statistics to be used for the gridded data.
  Options are 'mean', 'median', 'min', 'max', 'sd', 'var', 'sum'.

- write.out:

  logical. If TRUE, writes the output to a csv file. If FALSE returns
  the output as a dataframe. Default is FALSE.

- output.file:

  string. Full output file path (including .csv extension). Required if
  write.out is TRUE. Default is NULL.

## Value

A dataframe with appended values from the gridded data matching the
coordinates input, or writes a CSV if write.out is TRUE.
