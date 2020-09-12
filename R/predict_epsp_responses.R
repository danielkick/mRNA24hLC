#' @title Predict voltage response to a fictive epsp stimulus
#'
#' @description Takes current clamp step recording from `prep_abf()` and returns the predicted purely passive voltage response and diagnostic plots. Uses a hardcoded region of the protocol to establish the offset between the predicted and measured voltage traces.
#'
#' @param trace The prepared abf data.
#' @param tau The tau measured from a current clamp recording.
#' @param rin The membrane resitance from either a current or voltage clamp recording.
#' @param v_ch The voltage channel.
#' @param i_ch The current channel.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

predict_epsp_responses <- function(trace = trace_list$EPSP,
                                   tau = 19.8, #abs(mean(unlist(out$df[, "In9_Tau_est"]))),
                                   rin = 20.0, #abs(mean(unlist(out$df[, "In9_R1"]))),
                                   v_ch = "In9",
                                   i_ch = "In12"
){
  temp <- trace

  place_params_for_predict_voltage_trace(tau = tau,
                                         Rin = rin)

  predicted_response_list <- map(seq(1, length(unique(temp$Sweep))), function(i){
    place_data_for_predict_voltage_trace(input.df = temp[temp$Sweep == i, ],
                                         Time.ch = "Time",
                                         Inj.ch = i_ch)

    run_predict_voltage_trace_py()

    predicted_response <- retrieve_predicted_voltage()


    pred_v <- predicted_response[predicted_response$Time >= 19.38, "predicted"]
    meas_v <- temp[temp$Sweep == i & temp$Time >= 19.38, v_ch]
    offset <- median(unlist(pred_v - meas_v), na.rm = T)

    predicted_response$offset <- offset

    return(predicted_response)
  })



  my_plts <- map(
    seq_along(predicted_response_list),
    function(i){
      ggplot()+
        geom_line(data = predicted_response_list[[i]], aes(Time, predicted-offset), color = "cornflowerblue")+
        geom_line(data = temp[temp$Sweep == i, ], aes_string("Time", v_ch), color = "black")+
        labs(x = "Seconds", y = "mV")+
        coord_cartesian(y = c(-100, +100))
    })


  trace_plt <- cowplot::plot_grid(plotlist = my_plts)


  return(list(diagnostic_plots = list(trace = trace_plt),
              predictions = predicted_response_list))
}

