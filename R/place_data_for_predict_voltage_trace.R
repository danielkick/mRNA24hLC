#' @title Place Data for predict_voltage_trace.py
#'
#' @description This function places a sweep a current injection protocol in ./temp/ for predict_voltage_trace.py to use. It is recommended that one run \code{place_params_for_predict_voltage_trace()}, \code{place_data_for_predict_voltage_trace()}, \code{run_predict_voltage_trace.py()}, and \code{retrieve_predicted_voltage()} in that order.
#'
#' @param input.df A dataframe containing at least a time channel and a current injection channel.
#' @param Time.ch The Time channel to be used.
#' @param Inj.ch The channel with the current injected into the cell in nA.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export

place_data_for_predict_voltage_trace <- function(input.df = temp[temp$sweep==1, ],
                                                 Time.ch = "Time",
                                                 Inj.ch = "In7",
                                                 ...){
  input.df <- input.df[, c(Time.ch, Inj.ch)]
  names(input.df) <- c("Time", "Inj")
  write.csv(input.df,
            file = "./temp/current_inj.csv",
            row.names = F
  )
}
