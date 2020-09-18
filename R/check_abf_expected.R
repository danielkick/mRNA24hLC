#' @title Check ABF
#'
#' @description Takes an abf and checks that it's the expected size and contains the expected channels for the type specified in the metadata.
#'
#' @param exempt_ABF An array with the filenames of any ABFs that should be returned even if they don't meet expectations.
#' @param dir_path The directory path.
#' @param file_name The abf's name.
#' @param file_type The type of abf to be processed. Currently supports gjcc, gjvc, fi, and epsp
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples



check_abf_expected <- function(
  exempt_ABF = force_process_ABF, #c("000000_0000.abf")
  dir_path = abf_dir_path,
  file_name = "190808a_0018.abf", #as.character(abfs_one_ABF[1, "File"]),
  file_type = "gjcc" #as.character(abfs_one_ABF[1, "ABFType"])
){
  process_current_ABF <- F

  if(!file.exists(paste0(dir_path, file_name))){
    # write out couldn't find this, doesn't exist, into log
    warning(paste(file_name, "was not found in the specified location.\nRefer to /data/data_audits/RejectedABFs.csv"))

    FileRejectionReport <- data.frame(
      FilePath = dir_path,
      FileName = file_name,
      ABFType = NA,
      FileFound = F,
      ExpectedSize = NA,
      ExpectedSweeps = NA,
      AttemptTime = Sys.time(),
      ErrorMsg = NA
    )

    write.table(FileRejectionReport,
                file = here("data", "data_audits", "RejectedABFs.csv"),
                sep = ",",
                append = T,
                row.names = F)

    temp_trace <-  NA
    return(list(process = process_current_ABF, trace = temp_trace))

  } else {
    temp_trace_size <- gdata::humanReadable(file.size(paste0(dir_path, file_name)))
    temp_trace <- prep_abf(Path = dir_path, FileName = file_name)

    # Is the file the right size? Does it have the right number of sweeps?
    if(file_type == "gjvc"){
      expected_size <- temp_trace_size == "5.5 MiB"
      expected_sweeps <- length(unique(temp_trace$Sweep))==9
      # require_rin_tau <- T

    }else if(file_type == "gjcc"){
      expected_size <- temp_trace_size == "14.3 MiB"
      expected_sweeps <- length(unique(temp_trace$Sweep))==5
      # require_rin_tau <- T

    } else if(file_type == "epsp"){
      expected_size <- temp_trace_size == "3.8 MiB"
      expected_sweeps <- length(unique(temp_trace$Sweep))==4
      # require_rin_tau <- F

    } else if (file_type == "fi"){
      expected_size <- temp_trace_size == "45.8 MiB"
      expected_sweeps <- length(unique(temp_trace$Sweep))==11
      # require_rin_tau <- F
    }


    # Should we set `process_current_ABF` to TRUE?
    # Was everthing as expected OR is it in the "run anyway" list?
    if ((expected_size & expected_sweeps) | (file_name %in% exempt_ABF)){
      process_current_ABF <- T
    } else {
      # TODO If there was a problem write out to a log in data/data_audits/

      warning(paste(file_name, "was rejected. \nAdd it to `exempt_ABF` if you're sure it should be included. \nRefer to /data/data_audits/RejectedABFs.csv"))

      # write out a doc with those files not run named and why they were not run. Put this in data/data_audits

      FileRejectionReport <- data.frame(
        FilePath = dir_path,
        FileName = file_name,
        ABFType = file_type,
        FileFound = T,
        ExpectedSize = expected_size,
        ExpectedSweeps = expected_sweeps,
        AttemptTime = Sys.time(),
        ErrorMsg = NA
      )

      write.table(FileRejectionReport,
                  file = here("data", "data_audits", "RejectedABFs.csv"),
                  sep = ",",
                  append = T,
                  row.names = F)
    }
    return(list(process = process_current_ABF, trace = temp_trace))
  }
}
