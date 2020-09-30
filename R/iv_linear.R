#' @title Current Voltage Linear Fit
#'
#' @description This is a helper function which takes current and voltage columns from a dataframe, selects the approximately linear portion of the curve (as set by max.mV and min.mV) and returns the linear fit.
#'
#' @param input.df dataframe containing current/voltage pairs from a voltage clamp step protocol.
#' @param min.mV The lower bound for the approximately linear portion of the curve.
#' @param max.mV The upper bound for the approximately linear portion of the curve.
#' @param ch.i The column represnting the current measurements.
#' @param ch.v The column representing the voltage measurements.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

iv_linear <- function(input.df = temp,
                      min.mV = -35,
                      max.mV = 5,
                      ch.i = "S2Max",
                      ch.v = "S1Max"){

  # input.df = temp
  # min.mV = -81
  # max.mV = -39
  # ch.i = "S2Max"
  # ch.v = "S1Max"

  input.df <- as.data.frame(input.df)

  # input.df <- input.df[!is.na(input.df[[ch.v]]), ]

  if (nrow(input.df[!is.na(input.df[[ch.v]]), ]) > 1 ){
    input.df <- input.df %>% rename(CH.I = ch.i) %>% rename(CH.V = ch.v)

    fm <- lm(CH.I ~ CH.V, input.df[input.df$CH.V >= min.mV & input.df$CH.V <= max.mV, ], na.action = "na.omit")

  }
  return(list(intercept = fm$coefficients[[1]],
              slope = fm$coefficients[[2]]))

}
