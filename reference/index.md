# Package index

## 2D Gridded Pipeline

Functions for spatial cropping, masking, and gridded anomalies.

- [`crop_nc_2d()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/crop_nc_2d.md)
  : Crops a 2D netCDF based on the extent of a shape file
- [`mask_nc_2d()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/mask_nc_2d.md)
  : Masks a 2D netCDF using a lower and upper value
- [`convert_2d_longitude_gridded()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/convert_2d_longitude_gridded.md)
  : Convert spatial object longitude to standard format
- [`make_2d_summary_gridded()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_summary_gridded.md)
  : Calculates summary statistics on a 2D spatial grid
- [`make_2d_climatology_gridded()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_climatology_gridded.md)
  : Calculates a gridded climatology from spatial data based on
  reference dates
- [`make_2d_anomaly_gridded()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_anomaly_gridded.md)
  : Provides a gridded anomaly based on a reference climatology
- [`make_2d_deg_day_gridded()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_deg_day_gridded.md)
  : Provides a gridded summary of degree-day family statistics

## 2D Timeseries Pipeline

Functions for regional aggregation and tabular anomalies.

- [`make_2d_summary_ts()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_summary_ts.md)
  : Calculates summary statistics of 2D gridded data as a time series by
  area
- [`make_2d_climatology_ts()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_climatology_ts.md)
  : Calculates a timeseries climatology based on reference dates
- [`make_2d_anomaly_ts()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_anomaly_ts.md)
  : Creates an anomaly timeseries from a climatology
- [`make_2d_deg_day_ts()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/make_2d_deg_day_ts.md)
  : Calculates timeseries degree-day family statistics from spatial data

## Coordinate Extraction

- [`extract_daily_coord()`](https://nefsc.github.io/READ_EDAB_Utilities/reference/extract_daily_coord.md)
  : Matches daily coordinates to gridded data
