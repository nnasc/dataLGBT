report_core <- function(proc_data,
                        idioma = c("pt", "en"),
                        sogi.filter = c("all", "lgbt", "gay", "lesbian",
                                        "bissexual", "travesti",
                                        "mul.trans", "hom.trans"),
                        var.by = "Violencia_Relacionada",
                        add.p = FALSE,
                        time.comp = c("ano", "mes", "dia", "NULL"),
                        graph.comp = TRUE) {

  idioma <- match.arg(idioma)
  sogi.filter <- match.arg(sogi.filter)
  time.comp <- match.arg(time.comp)

  if (!inherits(proc_data, "data_proc_pipe")) {
    stop("`proc_data` deve ser um objeto da classe 'data_proc_pipe'. Execute `data_proc()` primeiro.")
  }

  if (is.null(proc_data$data$final) || !is.data.frame(proc_data$data$final)) {
    stop("`proc_data$data$final` não foi encontrado ou é inválido.")
  }

  df <- proc_data$data$final

  logo_png <- system.file("extdata", "dataLGBT_logo.png", package = "dataLGBT")
  logo_gif <- system.file("extdata", "dataLGBT.gif", package = "dataLGBT")
  logo_path <- if (nzchar(logo_png)) logo_png else if (nzchar(logo_gif)) logo_gif else NA_character_

  allow_lgbt_tables <- sogi.filter %in% c("all", "lgbt")

  report <- list(
    data = list(raw = df),
    tables = list(t1 = NULL, t2 = NULL, t3 = NULL, t4 = NULL),
    graphs = list(g1 = NULL, g2 = NULL, g3 = NULL, g4 = NULL, g5 = NULL),
    text = list(t1 = NULL, t2 = NULL, t3 = NULL, t4 = NULL,
                g1 = NULL, g2 = NULL, g3 = NULL, g4 = NULL, g5 = NULL),
    log = list(),
    steps = character(),
    meta = list(
      idioma = idioma,
      sogi.filter = sogi.filter,
      allow_lgbt_tables = allow_lgbt_tables,
      var.by = var.by,
      add.p = isTRUE(add.p),
      time.comp = if (time.comp == "NULL") NULL else time.comp,
      graph.comp = isTRUE(graph.comp),
      logo_path = logo_path,
      created_at = Sys.time()
    )
  )

  class(report) <- "dataLGBT_report"
  report
}
