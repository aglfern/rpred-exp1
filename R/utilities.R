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
