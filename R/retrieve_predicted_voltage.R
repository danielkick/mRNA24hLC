#' @title Retrieve Predicted Voltage
#'
#' @description This function retrieves the predicted voltage response of a passive membrane with paramters defined in ./temp/. It is recommended that one run \code{place_params_for_predict_voltage_trace()}, \code{place_data_for_predict_voltage_trace()}, \code{run_predict_voltage_trace.py()}, and \code{retrieve_predicted_voltage()} in that order.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
retrieve_predicted_voltage <- function(){
  return(
    cbind(read.csv("./temp/time_steps.csv"),
          read.csv("./temp/predicted_voltage.csv"))
  )
}
