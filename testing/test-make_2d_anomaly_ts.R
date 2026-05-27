# -------------------------------------------------------------------------
# Test script for Optimized make_2d_anomaly_ts
# -------------------------------------------------------------------------
library(dplyr)

# 1. Generate Mock Pipeline Timeseries Data
# Mimics output derived from standard test spatial data (2026 dates, 'test_zone_A')
mock_ts_data <- data.frame(
  time = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04", "2026-01-05")),
  value = c(12.5, 14.0, 15.2, 13.8, 11.9),
  area = "test_zone_A",
  var.name = "BottomS",
  statistic = "mean"
)

# 2. Generate Mock Climatology
# Mimics a reference climatology for the same area/time period
mock_climatology <- data.frame(
  time = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04", "2026-01-05")),
  value = c(12.0, 12.0, 12.0, 12.0, 12.0),
  area = "test_zone_A"
)

# 3. Execute Optimized Function
result_ls <- make_2d_anomaly_ts(
  data.in = mock_ts_data,
  climatology = mock_climatology,
  write.out = FALSE
)

# 4. Output Validation
print(result_ls[[1]])
