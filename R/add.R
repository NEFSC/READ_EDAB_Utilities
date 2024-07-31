#' Adds two numbers
#'
#' descriptions
#'
#' @param x numeric. First number
#' @param y numeric. Second number
#'
#' @return sum of values
#' 
#' @export

add <- function(x,y){
  
  #z <- x + y
  
  z <- base::sum(x,y)
  
  
  return(z)
}