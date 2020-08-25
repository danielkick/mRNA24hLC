#' @title Downsample Data
#'
#' @description This function downsamples a dataframe to the length requested.
#'
#' @param df The dataframe to be used.
#' @param len The length (number of rows) of observations desired.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
downsample_data <- function(df = temp,
                            len = 5000){
  return(df[ceiling(seq(1, nrow(df), length.out = len)), ])
}
