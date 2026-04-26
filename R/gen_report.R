#' @export
gen_report <- function(proc_data,
                       idioma = c("pt", "en"),
                       sogi.filter = c("all", "lgbt"),
                       var.by = "Violencia_Relacionada",
                       add.p = FALSE,
                       time.comp = c("ano", "mes", "dia", "none"),
                       graph.comp = TRUE,
                       export = TRUE,
                       format = c("all", "html", "pdf")) {

  idioma <- match.arg(idioma)
  sogi.filter <- match.arg(sogi.filter)
  time.comp <- match.arg(time.comp)
  format <- match.arg(format)

  # -------------------------------------------------------
  # 1. CORE
  # -------------------------------------------------------
  report <- report_core(
    proc_data = proc_data,
    idioma = idioma,
    sogi.filter = sogi.filter,
    var.by = var.by,
    add.p = add.p,
    time.comp = time.comp,
    graph.comp = graph.comp
  )

  # -------------------------------------------------------
  # 2. GARANTIR CID (CRÍTICO)
  # -------------------------------------------------------
  if (!all(c("APVP", "AVCI", "Grupo_CID") %in% names(report$data$raw))) {
    report$warnings <- c(
      report$warnings,
      "CID step não aplicado — algumas tabelas podem ser afetadas."
    )
  }

  # -------------------------------------------------------
  # 3. PIPELINE
  # -------------------------------------------------------
  report <- .dataLGBT_run_report_step(report, "step_report_overview")
  report <- .dataLGBT_run_report_step(report, "step_report_table1")
  report <- .dataLGBT_run_report_step(report, "step_report_table2")
  report <- .dataLGBT_run_report_step(report, "step_report_table3")
  report <- .dataLGBT_run_report_step(report, "step_report_table4")

  # gráficos dependentes de tempo
  if (graph.comp && time.comp != "none") {
    report <- .dataLGBT_run_report_step(report, "step_report_graph1")
    report <- .dataLGBT_run_report_step(report, "step_report_graph2")
    report <- .dataLGBT_run_report_step(report, "step_report_graph3")
  }

  if (graph.comp) {
    report <- .dataLGBT_run_report_step(report, "step_report_graph4")
    report <- .dataLGBT_run_report_step(report, "step_report_graph5")
  }

  # -------------------------------------------------------
  # 4. EXPORT (QUARTO)
  # -------------------------------------------------------
  if (isTRUE(export)) {

    base_name <- paste0(
      "dataLGBT_report_",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )

    output <- .dataLGBT_write_report_qmd(
      report,
      output_file = base_name,
      format = format
    )

    report$meta$output <- output

    return(invisible(list(
      report = report,
      output = output
    )))
  }

  # -------------------------------------------------------
  # 5. RETURN
  # -------------------------------------------------------
  return(report)
}
