#' @title Read ABF as Matrix
#'
#' @description This function is depricated. It's recommended that one use \code{readABF_as_matrix2()}. It has been retained for it's use in a visualizaito of "./extdata/epsp_stim_files/08 current injection.abf".
#'
#' @param path File path to the abf to be read in.
#' @param channels An array of the channels to be returned. Note that "all" is not an acceptable option here.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export

readABF_as_matrix <- function(
  path = "",
  channels = c("IN 4", "IN 9")){

  trace <- readABF::readABF(file = path)

  start.time <- trace$header$recTime[1]
  end.time <- trace$header$recTime[2]
  obs <- nrow(trace$data[[1]])

  temp <- trace$data[[1]][, (trace$channelNames %in% channels)]
  temp <- as.matrix(temp)

  colnames(temp) <- trace$channelNames[trace$channelNames %in% channels]
  temp <- cbind(temp, Time = seq(from = start.time,
                                 to = end.time,
                                 length.out = obs))

  return(temp)
}
