#' Masks a 2D netCDF using a lower and upper value
#'
#' descriptions
#'
#' @param data.in Either a character vector of full input file names, a list of SpatRasters, or a single SpatRaster (with one or multiple layers)
#' @param output.files character vector of full output file names corresponding to each input item. Required if write.out is TRUE.
#' @param shp.file  string, SpatVector, or SpatRaster. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract 
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @param min.value numeric. Minimum value of var.name 
#' @param max.value numeric. maximum value of var.name
#' @param binary logical. Whether mask should be binary (0 for outside range, 1 for inside range) or the original values
#' @param area.names character vector. specific area names to filter the shapefile
#'
#' @return A list of SpatRasters (if write.out=F)
#' 
#' @export

mask_nc_2d <- function(data.in, write.out = F, output.files = NULL, shp.file = NA, var.name, min.value, max.value, binary = F, area.names = NA){
  
  # --- Refactor Step 1: Normalize Input to List ---
  # This handles the user's requested 3 cases:
  # 1) Vector of files -> converted to list of files
  # 2) Single SpatRaster (multi-layer) -> converted to list of 1 SpatRaster (processed as a block)
  # 3) List of SpatRasters -> kept as list
  
  if (inherits(data.in, "SpatRaster")) {
    data.ls = list(data.in)
  } else if (is.character(data.in)) {
    data.ls = as.list(data.in)
  } else if (is.list(data.in)) {
    data.ls = data.in
  } else {
    stop("data.in must be a vector of file names, a list of SpatRasters, or a single SpatRaster.")
  }
  
  # Check output files alignment if writing
  if(write.out){
    if(is.null(output.files) || length(output.files) != length(data.ls)){
      stop("If write.out is TRUE, output.files must be provided and match the number of input items (1 output file per input object/file).")
    }
  }
  
  # --- Shapefile Handling ---
  if(inherits(shp.file, c('SpatVector','SpatRaster'))){
    shp.vect = shp.file
    use.shp = T
  }else if(is.character(shp.file) && !is.na(shp.file)){
    shp.vect = terra::vect(shp.file)
    use.shp = T
  }else{
    use.shp = F
  }
  
  # Filter shapefile by area names
  if(use.shp && all(!is.na(area.names))){
    shp.str = as.data.frame(shp.vect)
    # Find column containing all area.names (robust check)
    which.att = which(apply(shp.str, 2, function(x) all(area.names %in% x)))
    
    if(length(which.att) > 0){
      which.area = match(area.names, shp.str[,which.att[1]])
      shp.vect = shp.vect[which.area]  
    }
  }
  
  out.ls = list()
  
  # --- Process Data ---
  for(i in 1:length(data.ls)){
    
    item = data.ls[[i]]
    
    # Resolve input item to SpatRaster
    if(is.character(item)){
      data = terra::rast(item)
    } else if(inherits(item, "SpatRaster")){
      data = item
    } else {
      stop("Input list elements must be file paths or SpatRaster objects.")
    } 
    
    # Crop and Mask
    if(use.shp){
      data = terra::crop(data, shp.vect)
      data = terra::mask(data, shp.vect) 
    }
    
    # Clamp data (Masking by value range)
    # values=F sets outside values to NA
    data.mask = terra::clamp(data, lower = min.value, upper = max.value, values = F)
    
    if(binary){
      # Create binary mask (1 for valid, NA for invalid)
      data.mask = (data.mask * 0) + 1
      data.out = data.mask
    }else{
      data.out = data.mask
    }
    
    if(write.out){
      terra::writeCDF(data.out, output.files[i], varname = var.name, overwrite = TRUE)
    }else{
      out.ls[[i]] = data.out
    }
  }
  
  if(write.out == F){
    return(out.ls)  
  }
}