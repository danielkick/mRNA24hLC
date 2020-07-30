#' @title Print Shrug
#'
#' @description This function prints a shrug emoji a specified number of times, provided the input value is a numeric greater than zero.
#'
#' @param n how many shrugs should be printed
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples
#' shrug(5)

shrug <- function(n = 1, ...){
  if (is.numeric(n) & n>0){
    for (i in seq(1, n)){
        cat("¯\\_(ツ)_/¯\n")
    }
  }
}
