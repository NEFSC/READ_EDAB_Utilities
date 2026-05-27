# Calculates summary statistics on a 2D spatial grid

This function aggregates temporal spatial data (like daily rasters) into
broader time periods (e.g., months, seasons) and calculates specified
summary statistics. It can optionally mask the input to specific
shapefile regions before summarization.

## Usage

``` r
make_2d_summary_gridded(
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
  spatial data to be processed.

- var.name:

  character. Variable name you wish to extract and process.

- statistics:

  character vector. Which statistic(s) to calculate (e.g., c("mean",
  "max")).

- agg.time:

  character. Time scale to aggregate over (e.g., "days", "months",
  "years", "season").

- file.time:

  character. Time scale of the input files ('daily', 'monthly',
  'annual'). Default is 'annual'.

- shp.file:

  character, SpatVector, SpatRaster, or NA. Shapefile or raster to mask
  the input data to. Default is NA.

- area.names:

  character vector or NULL. Names of shapefile areas you want to retain.
  Default is NULL.

- tz:

  character or NA. Time zone to convert dates to. No correction if NA.
  Default is NA.

- touches:

  logical. If TRUE, all cells touched by lines or polygons will be
  masked, not just those on the center point. Default is TRUE.

- output.files:

  character vector or NULL. Full output file paths corresponding to each
  input file if write.out is TRUE. Default is NULL.

- write.out:

  logical. If TRUE, writes NetCDF files. If FALSE, returns a list of
  SpatRasterDatasets. Default is FALSE.

## Value

If write.out is TRUE, writes NetCDF files to disk. If FALSE, returns a
named list containing SpatRasterDatasets representing the summarized
data.
