# Calculates summary statistics of 2D gridded data as a time series by area

This function extracts spatial raster data across specified shapefile
regions and aggregates it temporally to produce timeseries summary
statistics. It processes inputs by grouping them (e.g., aggregating
daily layers into annual time series) and outputs either a list of
summarized data frames or writes RDS files directly.

## Usage

``` r
make_2d_summary_ts(
  data.in,
  var.name,
  statistics,
  agg.time,
  file.time = "annual",
  shp.file = NA,
  area.names = NULL,
  tz = NA,
  touches = TRUE,
  output.files = NULL,
  write.out = FALSE
)
```

## Arguments

- data.in:

  character vector, list, or SpatRaster. Single file path, vector of
  file paths, single SpatRaster, or list of SpatRasters representing the
  spatial data.

- var.name:

  character. Variable name you wish to extract.

- statistics:

  character vector. Which statistic(s) to calculate (e.g., c("mean",
  "max")).

- agg.time:

  character. Time scale to calculate aggregation over (e.g., "days",
  "months", "season", "years").

- file.time:

  character. What time scale the input files are on ('daily', 'monthly',
  'annual'). Default is 'annual'.

- shp.file:

  character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask
  the input data to. Default is NA.

- area.names:

  character vector or NULL. Names of the areas in the shapefile to
  extract. Default is NULL.

- tz:

  character or NA. Time zone to convert. No correction if NA. Default is
  NA.

- touches:

  logical. If TRUE, all cells touched by lines or polygons will be
  masked, not just those on the center point. Default is TRUE.

- output.files:

  character vector or NULL. Full output file paths corresponding to each
  processing group. Default is NULL.

- write.out:

  logical. If TRUE, writes RDS files. If FALSE, returns a list of data
  frames. Default is FALSE.

## Value

If write.out is TRUE, writes RDS files to disk. If FALSE, returns a list
of data frames summarized by timestep for each area.
