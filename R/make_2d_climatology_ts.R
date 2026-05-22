#' Calculates a timeseries climatology based on reference dates
#'
#' This function filters input timeseries data.frames by a start and stop time, binds them together, and calculates a summary statistic grouped by time, area, and variable.
#'
#' @param data.in character vector, list, or data.frame. Single RDS file path, vector of RDS file paths, single data.frame, or list of data.frames representing the timeseries data.
#' @param start.time numeric or character. The starting time value to filter the aggregated data.
#' @param stop.time numeric or character. The stopping time value to filter the aggregated data.
#' @param statistic character. The statistic over which to calculate the climatology (e.g., "mean", "max").
#' @param output.files character vector or NULL. Full output file path(s) for the RDS file if write.out is TRUE. Default is NULL.
#' @param write.out logical. If TRUE, writes an RDS file. If FALSE, returns a named list containing the climatology data.frame. Default is FALSE.
#'
#' @return If write.out is TRUE, writes an RDS file to disk. If FALSE, returns a named list containing the summarized climatology data.frame.
#' 
#' @export
make_2d_climatology_ts <- function(data.in, start.time, stop.time, statistic, output.files = NULL, write.out = FALSE) {
  
  # Standardize data.in
  if (inherits(data.in, "data.frame")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    if (!all(file.exists(data.in))) stop("One or more paths in data.in do not exist.")
    data.ls <- as.list(data.in)
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "data.frame"))) {
    data.ls <- data.in
  } else {
    stop("data.in must be an RDS file path, a vector of file paths, a single data.frame, or a list of data.frames.")
  }
  
  # OPTIMIZATION: Vectorized data retrieval and filtering
  out.ls <- lapply(data.ls, function(x) {
    df <- if (is.character(x)) readRDS(x) else x
    df |> dplyr::filter(time >= start.time & time <= stop.time)
  })
  
  # Bind all rows into a single data frame
  clim_bound <- dplyr::bind_rows(out.ls)
  
  # OPTIMIZATION: Edge case handling for zero-row subsets
  if (nrow(clim_bound) == 0) {
    stop("No rows remained after filtering with the provided start.time and stop.time.")
  }
  
  # Note: `statistic` is purposefully removed from group_by and mutated directly 
  # to reflect the *new* stat, avoiding carry-over confusion from the summary step.
  clim.out <- clim_bound |>
    dplyr::group_by(time, agg.time, var.name, area) |>
    dplyr::summarise(value = match.fun(statistic)(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(statistic = statistic) 
  
  if (write.out) {
    if (is.null(output.files)) stop("output.files must be provided when write.out is TRUE.")
    out_dir <- dirname(output.files[1])
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    saveRDS(clim.out, output.files[1])
  } else {
    out_list <- list(clim.out)
    names(out_list) <- paste0("climatology_ts_", statistic)
    return(out_list)
  }
}