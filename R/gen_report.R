#' @export
gen_report <- function(proc_data,
                       idioma = c("pt", "en"),
                       sogi.filter = c("all", "lgbt"),
                       var.by = "Violencia_Relacionada",
                       add.p = FALSE,
                       time.comp = c("ano", "mes", "dia", "none"),
                       graph.comp = TRUE,
                       export = FALSE) {

  idioma <- match.arg(idioma)
  sogi.filter <- match.arg(sogi.filter)
  time.comp <- match.arg(time.comp)

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
    report$warnings <- c(report$warnings, "CID step não aplicado — algumas tabelas podem ser afetadas.")
  }

  # -------------------------------------------------------
  # 3. PIPELINE
  # -------------------------------------------------------
  report <- .dataLGBT_run_report_step(report, "step_report_overview")
  report <- .dataLGBT_run_report_step(report, "step_report_table1")
  report <- .dataLGBT_run_report_step(report, "step_report_table2")
  report <- .dataLGBT_run_report_step(report, "step_report_table3")
  report <- .dataLGBT_run_report_step(report, "step_report_table4")

  # -------------------------------------------------------
  # 4. GRÁFICOS (com proteção de time.comp)
  # -------------------------------------------------------
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
  # 5. EXPORT (OPCIONAL)
  # -------------------------------------------------------
  if (export) {

    pdf_name <- paste0(
      "dataLGBT_report_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".pdf"
    )

    pdf_file <- .dataLGBT_write_report_rmd(
      report,
      output_file = pdf_name
    )

    report$meta$output_pdf <- pdf_file

    return(invisible(list(
      report = report,
      pdf_file = pdf_file
    )))
  }

  # -------------------------------------------------------
  # 6. RETURN
  # -------------------------------------------------------
  return(report)
}
