# Creates an anomaly timeseries from a climatology

This function calculates a timeseries anomaly by subtracting a reference
climatology from an input timeseries dataset. It standardizes dataframe
and RDS file inputs and either returns a list of anomaly dataframes to
the R environment or writes them directly to disk.

## Usage

``` r
make_2d_anomaly_ts(
  data.in,
  climatology,
  write.out = FALSE,
  output.files = NULL
)
```

## Arguments

- data.in:

  character vector, list, or data.frame. Single file path, vector of
  file paths, single data.frame, or list of data.frames representing the
  raw timeseries data.

- climatology:

  data.frame. A dataframe with the same time dimensions and area names
  as data.in to act as a climatological reference.

- write.out:

  logical. If TRUE, writes an RDS file. If FALSE, returns a list of
  data.frames. Default is FALSE.

- output.files:

  character vector. Full output file names corresponding to each input
  file. Required if write.out is TRUE. Default is NULL.

## Value

A list of anomaly data.frames if write.out is FALSE, or writes RDS files
to disk if TRUE.
