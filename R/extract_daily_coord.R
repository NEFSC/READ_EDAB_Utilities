#' Matches daily coordinates to gridded data
#'
#' This function extracts values from a daily or multi-layer gridded dataset at specific coordinate and time locations. It standardizes inputs and optionally computes focal statistics within a specified search radius around the target coordinates.
#'
#' @param data.in character vector, list, or SpatRaster. Single file path, vector of file paths, single SpatRaster, or list of SpatRasters.
#' @param coord.df data.frame. A dataframe containing 'lat', 'lon', and 'date' columns to be extracted from the gridded data.
#' @param var.name string. The name of the variable being extracted.
#' @param search.radius numeric. The number of cell "rings" around the closest match to aggregate over. 0 = closest cell, 1 = 3x3 cells around closest, etc.
#' @param statistics character vector. The statistics to be used for the gridded data. Options are 'mean', 'median', 'min', 'max', 'sd', 'var', 'sum'.
#' @param write.out logical. If TRUE, writes the output to a csv file. If FALSE returns the output as a dataframe. Default is FALSE.
#' @param output.file string. Full output file path (including .csv extension). Required if write.out is TRUE. Default is NULL.
#' 
#' @return A dataframe with appended values from the gridded data matching the coordinates input, or writes a CSV if write.out is TRUE.
#' 
#' @export
extract_daily_coord <- function(data.in, coord.df, var.name, search.radius = 0, statistics = "mean", write.out = FALSE, output.file = NULL) {
  
  # Data Input Standardization
  if (inherits(data.in, "SpatRaster")) {
    data.ls <- list(data.in)
  } else if (is.character(data.in)) {
    data.ls <- as.list(data.in)
    if (!all(grepl('.nc', unlist(data.ls)))) {
      stop("All input files must be netCDF (.nc) files.")
    }
  } else if (is.list(data.in) && all(sapply(data.in, inherits, "SpatRaster"))) {
    data.ls <- data.in
  } else {
    stop("data.in must be a file path, a vector of file paths, a single SpatRaster, or a list of SpatRasters.")
  }
  
  # Ensure standard coordinate data structure
  if (!all(c("lat", "lon", "date") %in% names(coord.df))) {
    stop("coord.df must contain 'lat', 'lon', and 'date' columns.")
  }
  coord.df$date <- as.Date(coord.df$date)
  
  out.ls <- list()
  
  for (i in seq_along(data.ls)) {
    
    if (is.character(data.ls[[i]])) {
      if (!file.exists(data.ls[[i]])) stop(sprintf("File does not exist: %s", data.ls[[i]]))
      this.data <- terra::rast(data.ls[[i]], subds = var.name)
    } else {
      this.data <- data.ls[[i]]
    }
    
    # Time handling and fallback
    this.data.time <- terra::time(this.data)
    if (all(is.na(this.data.time)) || is.null(this.data.time)) {
      if (is.character(data.ls[[i]])) {
        file.date <- suppressWarnings(as.Date(gsub(".*(\\d{4})-(\\d{2})-(\\d{2}).*", "\\1-\\2-\\3", basename(data.ls[[i]]))))
        if (is.na(file.date)) stop(sprintf("Cannot determine time for %s", data.ls[[i]]))
        this.data.time <- rep(file.date, terra::nlyr(this.data))
      } else {
        stop("Cannot determine time for SpatRaster input.")
      }
    }
    this.data.time <- as.Date(this.data.time)
    
    # Pre-filter spatial intersections to avoid costly empty extractions
    valid_coords <- coord.df[coord.df$date %in% this.data.time, ]
    if (nrow(valid_coords) == 0) next
    
    pts <- as.matrix(valid_coords[, c("lon", "lat")])
    central_cells <- terra::cellFromXY(this.data, pts)
    
    # Edge case: Keep only points that successfully mapped to the raster grid
    valid_idx <- !is.na(central_cells)
    if (!any(valid_idx)) next
    
    valid_coords <- valid_coords[valid_idx, ]
    central_cells <- central_cells[valid_idx]
    pts <- pts[valid_idx, , drop = FALSE]
    
    val.product.coords <- terra::xyFromCell(this.data, central_cells)
    
    # Vectorized subsetting of layer targets instead of looping through dates
    layer_idx <- match(valid_coords$date, this.data.time)
    
    out.match <- data.frame(
      lon.obs = pts[, 1],
      lat.obs = pts[, 2],
      lon.product = val.product.coords[, 1],
      lat.product = val.product.coords[, 2],
      date = valid_coords$date,
      var.name = var.name,
      search.radius = search.radius,
      center.cell = central_cells,
      stringsAsFactors = FALSE
    )
    
    # Extract values directly from Terra matrix representation for massive speedups
    all_center_vals <- this.data[central_cells]
    out.match$center.value <- all_center_vals[cbind(seq_along(central_cells), layer_idx)]
    
    if (search.radius == 0) {
      out.ls[[length(out.ls) + 1]] <- out.match
    } else {
      # Highly optimized search radius geometry generation
      rc <- terra::rowColFromCell(this.data, central_cells)
      max_row <- terra::nrow(this.data)
      max_col <- terra::ncol(this.data)
      
      stat_results <- lapply(seq_along(central_cells), function(pt_idx) {
        r <- rc[pt_idx, 1]
        c <- rc[pt_idx, 2]
        lyr <- layer_idx[pt_idx]
        
        rows <- max(1, r - search.radius):min(max_row, r + search.radius)
        cols <- max(1, c - search.radius):min(max_col, c + search.radius)
        
        # Calculate cell bounding box limits natively
        grid <- expand.grid(row = rows, col = cols)
        cells <- terra::cellFromRowCol(this.data, grid$row, grid$col)
        
        box_vals <- this.data[[lyr]][cells]
        if (is.data.frame(box_vals)) box_vals <- box_vals[[1]]
        
        res <- lapply(statistics, function(stat) {
          val_stat <- switch(stat,
                             "mean" = mean(box_vals, na.rm = TRUE),
                             "median" = median(box_vals, na.rm = TRUE),
                             "min" = min(box_vals, na.rm = TRUE),
                             "max" = max(box_vals, na.rm = TRUE),
                             "sd" = sd(box_vals, na.rm = TRUE),
                             "var" = var(box_vals, na.rm = TRUE),
                             "sum" = sum(box_vals, na.rm = TRUE),
                             NA
          )
          data.frame(pt_id = pt_idx, statistic = stat, value = val_stat, stringsAsFactors = FALSE)
        })
        do.call(rbind, res)
      })
      
      stat_df <- do.call(rbind, stat_results)
      
      out.match$pt_id <- seq_len(nrow(out.match))
      merged_df <- merge(out.match, stat_df, by = "pt_id")
      merged_df$pt_id <- NULL 
      
      out.ls[[length(out.ls) + 1]] <- merged_df
    }
  }
  
  if (length(out.ls) == 0) return(data.frame())
  
  output.df <- dplyr::bind_rows(out.ls)
  
  if (write.out) {
    if (is.null(output.file)) stop("output.file must be provided when write.out is TRUE.")
    out_dir <- dirname(output.file)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    write.csv(output.df, output.file, row.names = FALSE)
  } else {
    return(output.df)
  }
}