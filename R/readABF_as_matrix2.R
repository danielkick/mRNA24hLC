#' @title Read ABF as Matrix (version) 2
#'
#' @description This function returns a matrix with Time/Value measurements from an Axon Binary File. Version 2 works on recordings with multiple sweeps whereas version 1 would be confused by multiple sweeps.
#'
#' @param path File path to the abf to be read in
#' @param channels Which channels should be returned? Note that "all" is an acceptable option.
#' @param relative.time If \code{TRUE}, then the time channel will begin at 0. Otherwise the minimum will be the minimum time in the file.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples
#'
readABF_as_matrix2 <- function(
  path = "",
  channels = c("IN 4", "IN 9"), # "all" is also acceptable
  relative.time = T
){

  trace <- readABF::readABF(file = path)

  start.time <- trace$header$recTime[1]
  end.time <- trace$header$recTime[2]

  temp_list <- map(seq(1, length(trace$data)), function(i){
    # obs <- nrow(trace$data[[i]])

    if (mean(channels %in% trace$channelNames) == 1){
      temp <- trace$data[[i]][, (trace$channelNames %in% channels)]
      temp <- cbind(temp, rep(i, nrow(temp)))
      temp <- as.matrix(temp)
      colnames(temp) <- c(trace$channelNames[trace$channelNames %in% channels], "Sweep")
      return(temp)
    } else if (channels == "all"){
      temp <- trace$data[[i]]
      temp <- cbind(temp, rep(i, nrow(temp)))
      temp <- as.matrix(temp)
      colnames(temp) <- c(trace$channelNames, "Sweep")

      return(temp)
    } else {
      current_options <- paste(as.array(trace$channelNames), sep =", ")
      warning(paste("Provided Channels do not exist. \nPlease select 'all' or select from the following existing channels:\n"))
      warning(current_options)

      temp <- NA
      return(temp)
    }

  })

  if(sum(is.na(temp_list)) > 0){
    warning("Exiting due to missing Sweep or improper channel selection.")
    return(NULL)
  } else {
    temp <- do.call(rbind, temp_list)

  }

  if (relative.time == F){
    temp <- cbind(temp, Time = seq(from = start.time,
                                   to = end.time,
                                   length.out = nrow(temp)))
    return(temp)
  } else if (relative.time == T){
    temp <- cbind(temp, Time = seq(from = 0,
                                   to = end.time-start.time,
                                   length.out = nrow(temp)))
    return(temp)
  } else {
    warning("Exiting due to 'relative.time' being neither 'T' or 'F'.")
    return(NULL)
  }
}
