# This global constant must be defined by the main project function
# It defines the log level that should be generated.
# The lower the number the less number of log output is generated.
LOG_LEVEL <- 2

#' Generates log
#'
#' @param texto
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
generate_log <- function(texto,nivel=1) {
   if (nivel <= LOG_LEVEL )
      print(paste(Sys.time(), ":", texto))
}



library(corrplot)
library(plyr) # antes de Hmisc e dplyr
library(Hmisc) # antes de dplyr e fields
library(tidyr) # antes de sets
library(sets) # antes de dplyr e data.table
library(dplyr) # precisa vir antes do data.table
library(data.table)
library(doParallel)
library(fields)
library(lsr)
library(MLmetrics)
library(nortest)
library(readr)
library(writexl)
