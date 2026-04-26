# =========================
# INTERNAL: RUN REPORT STEP
# =========================

.dataLGBT_run_report_step <- function(report, step_name) {

  if (!inherits(report, "dataLGBT_report")) {
    stop("`report` deve ser um objeto da classe 'dataLGBT_report'.", call. = FALSE)
  }

  if (!is.character(step_name) || length(step_name) != 1L) {
    stop("`step_name` deve ser uma string única.", call. = FALSE)
  }

  # Procura a função dentro do namespace do pacote
  if (!exists(step_name, envir = asNamespace("dataLGBT"), mode = "function")) {
    stop(sprintf("Não foi possível encontrar a função '%s'.", step_name), call. = FALSE)
  }

  step_fun <- get(step_name, envir = asNamespace("dataLGBT"), mode = "function")

  out <- tryCatch(
    step_fun(report),
    error = function(e) {
      stop(
        sprintf("Falha no step '%s': %s", step_name, conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  if (!inherits(out, "dataLGBT_report")) {
    stop(
      sprintf("O step '%s' não retornou um objeto da classe 'dataLGBT_report'.", step_name),
      call. = FALSE
    )
  }

  out$steps <- c(out$steps, step_name)

  if (is.null(out$log)) {
    out$log <- list()
  }
  out$log[[step_name]] <- list(
    timestamp = Sys.time(),
    status = "ok"
  )

  return(out)
}
