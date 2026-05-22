# Optimized Implementation Test Script
# Load required libraries
library(dplyr)
library(EDABUtilities)

# Generate mock data.frame that mimics the expected pipeline outputs 
# derived from the standard test spatial data
mock_ts_data <- data.frame(
  time = as.Date(c('2026-01-01', '2026-01-02', '2026-01-03', '2026-01-04', '2026-01-05')),
  value = c(15.5, 16.2, 14.8, 17.1, 15.9),
  area = rep("test_zone_A", 5),
  var.name = rep("temperature", 5),
  agg.time = rep("days", 5),
  statistic = rep("mean", 5) 
)

# Execute the optimized function
opt_ts_climatology <- make_2d_climatology_ts(
  data.in = mock_ts_data,
  start.time = as.Date("2026-01-02"),
  stop.time = as.Date("2026-01-04"),
  statistic = "max",
  write.out = FALSE
)

# View resulting output (should perfectly match the conformed execution)
print(opt_ts_climatology)