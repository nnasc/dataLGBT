# =========================
# STEP: LINK SUMMARY
# =========================

step_link_summary <- function(save_summary = TRUE,
                              summary_path = "linkage_summary.txt",
                              thresholds = c(0.99,0.95,0.90,0.85,0.75,
                                             0.65,0.55,0.45,0.35,0.25,
                                             0.10,0.05,0.01)) {

  function(data) {

    if (is.null(data$linkage$model)) {
      stop("Modelo de linkage não encontrado. Execute `step_linkage()` antes.")
    }

    fl <- data$linkage$model

    summary_output <- capture.output({

      cat("=====================================\n")
      cat("      RELATÓRIO DE LINKAGE\n")
      cat("=====================================\n\n")

      cat("Data:", format(Sys.time()), "\n\n")

      # ----------------------
      # Confusion Matrix
      # ----------------------
      cat("---- Confusion Matrix ----\n")
      tryCatch({
        print(fastLink::confusion(fl))
      }, error = function(e) {
        cat("Erro em confusion():", e$message, "\n")
      })

      # ----------------------
      # Summary thresholds
      # ----------------------
      cat("\n---- Summary por Threshold ----\n")
      tryCatch({
        print(summary(fl, thresholds = thresholds))
      }, error = function(e) {
        cat("Erro em summary():", e$message, "\n")
      })

      # ----------------------
      # EM parameters
      # ----------------------
      cat("\n---- Parâmetros EM ----\n")
      if (!is.null(fl$EM)) {
        tryCatch({
          em_info <- fastLink::inspectEM(
            fl$EM,
            posterior.range = c(max(0, threshold - 0.10), min(1, threshold + 0.10)),
            digits = 2
          )
          print(em_info)
        }, error = function(e) {
          cat("Erro em inspectEM():", e$message, "\n")
        })
      } else {
        cat("Objeto EM não disponível.\n")
      }

      cat("\n=====================================\n")
    })

    # Salvar no pipe
    data$linkage$summary <- summary_output

    # Salvar em arquivo
    if (isTRUE(save_summary)) {
      tryCatch({
        writeLines(summary_output, con = summary_path)
      }, error = function(e) {
        warning("Falha ao salvar relatório: ", e$message)
      })
    }

    return(data)
  }
}
