#' Creates an anomaly timeseries from a climatology
#'
#' This function calculates a timeseries anomaly by subtracting a reference climatology from an input timeseries dataset. It standardizes dataframe and RDS file inputs and either returns a list of anomaly dataframes to the R environment or writes them directly to disk.
#'
#' @param data.in character vector, list, or data.frame. Single file path, vector of file paths, single data.frame, or list of data.frames representing the raw timeseries data.
#' @param climatology data.frame. A dataframe with the same time dimensions and area names as data.in to act as a climatological reference.
#' @param write.out logical. If TRUE, writes an RDS file. If FALSE, returns a list of data.frames. Default is FALSE.
#' @param output.files character vector. Full output file names corresponding to each input file. Required if write.out is TRUE. Default is NULL.
#'
#' @return A list of anomaly data.frames if write.out is FALSE, or writes RDS files to disk if TRUE.
#' 
#' @export
make_2d_anomaly_ts <- function(data.in, climatology, write.out = FALSE, output.files = NULL) {
  
  # Data Input Standardization
  if (inherits(data.in, "data.frame")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    data.ls <- as.list(data.in)
    if (!all(grepl('\\.rds$', tolower(unlist(data.ls))))) {
      stop("All input files must be .rds files.")
    }
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "data.frame"))) {
    data.ls <- data.in
  } else {
    stop("data.in must be a file path, a vector of file paths, a single data.frame, or a list of data.frames.")
  }
  
  if (!inherits(climatology, "data.frame")) {
    stop("climatology must be a data.frame.")
  }
  
  climatology <- dplyr::rename(climatology, ref.value = value)
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      if (!file.exists(data.ls[[i]])) stop(sprintf("File does not exist: %s", data.ls[[i]]))
      data <- readRDS(data.ls[[i]])
    } else {
      data <- data.ls[[i]]
    }
    
    if (nrow(data) == 0) {
      warning(sprintf("Input data frame %s is empty. Skipping.", i))
      next
    }
    
    # Optimization: Safely determine dynamic join columns to avoid noisy messages and 
    # prevent accidental mismatches if data/climatology share unrelated columns.
    join_cols <- intersect(names(data), names(climatology))
    join_cols <- join_cols[join_cols != "ref.value"]
    
    if (length(join_cols) == 0) {
      stop("No common columns found between data.in and climatology to perform a join.")
    }
    
    data.comb <- data |>
      dplyr::left_join(climatology, by = join_cols) |>
      dplyr::mutate(anom.value = value - ref.value)
    
    # Edge case: Alert the user if climatology frames don't perfectly align with raw data
    if (any(is.na(data.comb$anom.value) & !is.na(data.comb$value))) {
      warning(sprintf("Some observations in item %s did not match the climatology and resulted in NA anomalies.", i))
    }
    
    if (write.out) {
      if (is.null(output.files) || length(output.files) != length(data.ls)) {
        stop("output.files must be provided and match the length of data.in when write.out is TRUE.")
      }
      
      out_dir <- dirname(output.files[i])
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      saveRDS(data.comb, output.files[i])
    } else {
      out.ls[[i]] <- data.comb
    }
  }
  
  if (write.out == FALSE) {
    if (is.character(data.in)) {
      names(out.ls) <- basename(data.in)
    } else {
      names(out.ls) <- paste0("ts_", seq_along(out.ls))
    }
    return(out.ls)  
  }
}