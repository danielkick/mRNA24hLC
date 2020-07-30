#' @title Fit Tau with a One Term Exponential
#'
#' @description Fits a 1 term exponential. Designed to be used for finding a cell's tau.
#'
#' @param df A dataframe containing a Time channel and a Voltage channel. The Time channel should begin at 0 and the input region should contain a passive response to a current step and nothing else.
#' @param IV The input paramter for the right side of the function. Here Time should be used.
#' @param DV The output paramter on the left side of the formula. Here a voltage channel should be used.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples
#' fit_tau_1term_exp(
#' df = temp_in9,
#' IV = "Time",
#' DV = "In9")

fit_tau_1term_exp <- function(
  df = temp_in9,
  IV = "Time",
  DV = "In9"
){
  ### 1 term fit ####
  # ref
  # https://stackoverflow.com/questions/26560849/exponential-regression-with-nls-in-r
  # https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
  # https://rpubs.com/mengxu/exponential-model

  # If we want to extend it, here's the call to use.
  # fm <- nlsLM(
  #   In9 ~ a*exp(b*Time) + c*exp(d*Time)+ e,
  #   data = temp_in9,
  #   start = list(a = 1, b = -0.01, c = 1, d = -0.1, e = 10)
  #   )

  # and the error from the test case:
  # Error in nlsModel(formula, mf, start, wts) :
  #   singular gradient matrix at initial parameter estimates
  # In addition: Warning message:
  # In nls.lm(par = start, fn = FCT, jac = jac, control = control, lower = lower,  :
  #   lmdif: info = -1. Number of iterations has reached `maxiter' == 50.

  input_formula <- as.formula(
    paste0(DV, " ~ a*exp(b*", IV, ") + c")
  )

  fm <- nlsLM(
    input_formula,
    data = df,
    start = list(a = 1, b = -0.01, c = 10)
  )

  tidy_fm <- broom::tidy(fm)

  # Get fit to check goodness of fit
  check_fit <- df[, c(IV, DV)]
  check_fit$Fit <- as.numeric(tidy_fm[1, "estimate"])*exp(as.numeric(tidy_fm[2, "estimate"]) * check_fit[[IV]]) + as.numeric(tidy_fm[3, "estimate"])

  return(list(fit = tidy_fm,
              check_fit = check_fit))

}
