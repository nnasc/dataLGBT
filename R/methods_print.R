#' @export
print.data_link_pipe <- function(x, ...) {
  
  sinan_n <- if (!is.null(x$data$sinan_raw)) nrow(x$data$sinan_raw) else NA
  sim_n   <- if (!is.null(x$data$sim_raw)) nrow(x$data$sim_raw) else NA
  
  matches_n <- if (!is.null(x$data$linkage$matches)) {
    nrow(x$data$linkage$matches)
  } else {
    NA
  }
  
  cat("\n==============================\n")
  cat("      data_link pipeline\n")
  cat("==============================\n\n")
  
  cat("SINAN:", format(sinan_n, big.mark = ","), "\n")
  cat("SIM:  ", format(sim_n, big.mark = ","), "\n\n")
  
  cat("Matches:", format(matches_n, big.mark = ","), "\n\n")
  
  cat("Steps:\n")
  cat(paste(x$steps, collapse = " -> "), "\n\n")
  
  invisible(x)
}