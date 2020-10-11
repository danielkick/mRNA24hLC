#' @title Within x IQR
#'
#' @description For a given column and multiplier, returns a logcal array of whether each value is within X*IQR +/- median.
#'
#' @param col_in The numeric column under consideration.
#' @param multiplier The multiplier desired. Default is 1.5.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

win_x_iqr <- function(col_in, multiplier = 1.5){
  col_in <- as.vector(col_in)

  X_low  <- median(col_in, na.rm = T) - (multiplier * IQR(col_in, na.rm = T))
  X_high <- median(col_in, na.rm = T) + (multiplier * IQR(col_in, na.rm = T))
  X_pass <- (col_in > X_low) * (col_in < X_high)

  return(as.logical(X_pass))
}
