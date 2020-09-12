#' @title Prepare ABF
#'
#' @description Prepares an ABF to be used with `process_gjvc_trace()`, `process_gjcc_trace()`, `predict_fi_responses()`, or `predict_epsp_responses()`. Also just a handy prep function.
#'
#' @param Path The directory containing at least one ABF.
#' @param FileName The ABF to be used.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples


prep_abf <- function(Path = abf_dir_path,
                     FileName = "190808a_0004.abf"){
  temp <- readABF_as_matrix2(path = paste0(Path, FileName),
                             channels = "all",
                             relative.time = T) %>%
    as.data.frame() %>%
    janitor::clean_names(case = "upper_camel") %>%
    mutate(Sweep = as.factor(Sweep)) %>%
    group_by(Sweep) %>%
    mutate(MinTime = min(Time, na.rm = T)) %>%
    mutate(Time = Time - MinTime) %>%
    select(-MinTime)
  return(temp)
}
