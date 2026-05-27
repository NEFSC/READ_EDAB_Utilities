# Calculates a timeseries climatology based on reference dates

This function filters input timeseries data.frames by a start and stop
time, binds them together, and calculates a summary statistic grouped by
time, area, and variable.

## Usage

``` r
make_2d_climatology_ts(
  data.in,
  start.time,
  stop.time,
  statistic,
  output.files = NULL,
  write.out = FALSE
)
```

## Arguments

- data.in:

  character vector, list, or data.frame. Single RDS file path, vector of
  RDS file paths, single data.frame, or list of data.frames representing
  the timeseries data.

- start.time:

  numeric or character. The starting time value to filter the aggregated
  data.

- stop.time:

  numeric or character. The stopping time value to filter the aggregated
  data.

- statistic:

  character. The statistic over which to calculate the climatology
  (e.g., "mean", "max").

- output.files:

  character vector or NULL. Full output file path(s) for the RDS file if
  write.out is TRUE. Default is NULL.

- write.out:

  logical. If TRUE, writes an RDS file. If FALSE, returns a named list
  containing the climatology data.frame. Default is FALSE.

## Value

If write.out is TRUE, writes an RDS file to disk. If FALSE, returns a
named list containing the summarized climatology data.frame.
