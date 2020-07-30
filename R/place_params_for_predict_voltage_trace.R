#' @title Place Parameters for predict_voltage_trace
#'
#' @description This function places tau and R_{in} into a text files in ./temp/ for predict_voltage_trace.py to use. It is recommended that one run \code{place_params_for_predict_voltage_trace()}, \code{place_data_for_predict_voltage_trace()}, \code{run_predict_voltage_trace.py()}, and \code{retrieve_predicted_voltage()} in that order.
#'
#' @param tau Tau of the cell.
#' @param Rin Input resistance of the cell.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
place_params_for_predict_voltage_trace <- function(tau = 10.0,
                                                   Rin = 1.0){
  write.table(tau, file = "./temp/measured_tau.txt", sep = "", row.names = F, col.names = F)
  write.table(Rin, file = "./temp/measured_Rin.txt", sep = "", row.names = F, col.names = F)
}
