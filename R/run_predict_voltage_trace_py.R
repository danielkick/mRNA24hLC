#' @title Run predict_voltage_trace.py
#'
#' @description Activates the base conda env and runs predict_voltage_trace.py. This requires conda and \code{brian2} installed in the base env to work. It has only been tested on Windows 10. The called python script will return the expected voltage based on the paramters in ./temp/. It is recommended that one run \code{place_params_for_predict_voltage_trace()}, \code{place_data_for_predict_voltage_trace()}, \code{run_predict_voltage_trace.py()}, and \code{retrieve_predicted_voltage()} in that order.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
run_predict_voltage_trace_py <- function(){
  # shell("conda activate base")
  # shell("python ./Python/predict_voltage_trace.py")

  # running above commands separately can cause
  #   Traceback (most recent call last):
  #   File "./Python/predict_voltage_trace.py", line 2, in <module>
  #     from brian2 import *
  # ModuleNotFoundError: No module named 'brian2'
  # Warning message:
  # In shell("python ./Python/predict_voltage_trace.py") :
  #   'python ./Python/predict_voltage_trace.py' execution failed with error code 1
  # Perhaps there is insufficent time for the first command to complete so the env is not activated.
  # Combined it seems to work.
  shell("conda activate base && python ./Python/predict_voltage_trace.py")
}
