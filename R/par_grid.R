#' creates grid with function values
#'
#' This function creates a grid, where x are the row-names, y are the column-names
#' and the grid-values are the function values at (x,y).
#' @param x vector of x-values
#' @param y vector of y-values
#' @param func function to create grid values
#' @param ... optional: arguments of func
#' @return length(x) x length(y) - matrix with function values evaluated at (x,y)
#' @export
#' @examples par_grid(seq(0,1,10),seq(0,1,11),sum)

par_grid <- function(x,y,func,...){
  z <- matrix(nrow=length(x), ncol=length(y))

  for(m in 1:length(x)){
    for(n in 1:length(y)){
      w <- c(x[m], y[n])
      z[m,n] <- func(w,...)
    }
  }
  return(z)
}
