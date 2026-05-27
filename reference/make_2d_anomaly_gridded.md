# Provides a gridded anomaly based on a reference climatology

This function calculates a spatial anomaly by subtracting a reference
climatology from a gridded dataset. It standardizes spatial inputs,
aligns resolutions, and outputs either a list of SpatRasters to the R
environment or writes the results directly to disk as NetCDF files.

## Usage

``` r
make_2d_anomaly_gridded(
  data.in,
  climatology,
  var.name,
  shp.file = NA,
  area.names = NA,
  write.out = FALSE,
  output.files = NULL
)
```

## Arguments

- data.in:

  character vector, list, SpatRaster, or SpatRasterDataset. Single file
  path, vector of file paths, single spatial object, or list of spatial
  objects representing the raw data.

- climatology:

  string or SpatRaster. The reference climatology to subtract from the
  input data. Should ideally be on the same resolution as data.in.

- var.name:

  string. Variable name you wish to extract and write.

- shp.file:

  string, SpatVector, or SpatRaster. The shapefile or spatial object
  used to crop/mask the data. Default is NA.

- area.names:

  character vector. Optional names of specific areas within the
  shapefile to filter by before masking. Default is NA.

- write.out:

  logical. If TRUE, writes a netCDF file. If FALSE, returns a list of
  SpatRasters. Default is FALSE.

- output.files:

  character vector. Full output file names corresponding to each input
  file. Required if write.out is TRUE. Default is NULL.

## Value

A list of SpatRasters containing the anomalies if write.out is FALSE, or
writes NetCDF files to disk if TRUE.
