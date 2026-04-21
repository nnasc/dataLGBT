#' @export
summary.data_link_pipe <- function(object, ...) {
  
  x <- object
  
  sinan_n <- if (!is.null(x$data$sinan_raw)) nrow(x$data$sinan_raw) else NA
  sim_n   <- if (!is.null(x$data$sim_raw)) nrow(x$data$sim_raw) else NA
  
  pairs_n <- if (!is.null(x$data$linkage$pairs)) nrow(x$data$linkage$pairs) else NA
  matches_n <- if (!is.null(x$data$linkage$matches)) nrow(x$data$linkage$matches) else NA
  
  cat("\n=====================================\n")
  cat("        data_link summary\n")
  cat("=====================================\n\n")
  
  cat("SINAN:", sinan_n, "\n")
  cat("SIM:  ", sim_n, "\n\n")
  
  cat("Pairs:   ", pairs_n, "\n")
  cat("Matches: ", matches_n, "\n")
  
  if (!is.na(pairs_n) && pairs_n > 0) {
    cat("Match rate:", round(matches_n / pairs_n, 4), "\n")
  }
  
  cat("\nSteps:\n")
  cat(paste(x$steps, collapse = " -> "), "\n\n")
  
  cat("Log:\n")
  for (nm in names(x$log)) {
    
    log_i <- x$log[[nm]]
    
    if (!is.null(log_i$sinan_n_before)) {
      cat(
        nm, ": ",
        log_i$sinan_n_before, "->", log_i$sinan_n_after,
        " | ",
        log_i$sim_n_before, "->", log_i$sim_n_after,
        "\n"
      )
    }
  }
  
  invisible(x)
}