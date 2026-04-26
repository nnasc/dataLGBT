report_core <- function(proc_data,
                        idioma = c("pt", "en"),
                        sogi.filter = c("all", "lgbt", "gay", "lesbian",
                                        "bissexual", "travesti",
                                        "mul.trans", "hom.trans"),
                        var.by = "Violencia_Relacionada",
                        add.p = FALSE,
                        time.comp = c("ano", "mes", "dia", "none"),
                        graph.comp = TRUE) {

  idioma <- match.arg(idioma)
  sogi.filter <- match.arg(sogi.filter)
  time.comp <- match.arg(time.comp)

  df <- NULL

  if (!is.null(proc_data$data$final) && is.data.frame(proc_data$data$final)) {
    df <- proc_data$data$final
  } else if (!is.null(proc_data$data) && is.data.frame(proc_data$data)) {
    df <- proc_data$data
  } else {
    stop("`proc_data$data` não foi encontrado ou é inválido.", call. = FALSE)
  }

  # -------------------------------------------------------
  # 1. PADRONIZAÇÃO BÁSICA (DEFENSIVA)
  # -------------------------------------------------------
  df <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character)
    )

  # Garantir colunas críticas (sem quebrar fluxo)
  if (!"Morte" %in% names(df) && "Obito" %in% names(df)) {
    df$Morte <- df$Obito
  }

  # -------------------------------------------------------
  # 2. APLICAR FILTRO SOGI (AGORA SIM FUNCIONAL)
  # -------------------------------------------------------
  if (sogi.filter != "all" && "SGM" %in% names(df)) {

    df$SGM <- as.character(df$SGM)

    if (sogi.filter == "lgbt") {
      df <- df[df$SGM != "Ignorado" & !is.na(df$SGM), , drop = FALSE]
    } else {
      df <- df[df$SGM == sogi.filter, , drop = FALSE]
    }
  }

  # -------------------------------------------------------
  # 3. NORMALIZAR time.comp
  # -------------------------------------------------------
  time.comp <- if (time.comp == "none") NULL else time.comp

  # -------------------------------------------------------
  # 4. LOGO
  # -------------------------------------------------------
  logo_png <- system.file("extdata", "dataLGBT_logo.png", package = "dataLGBT")
  logo_gif <- system.file("extdata", "dataLGBT.gif", package = "dataLGBT")

  logo_path <- if (nzchar(logo_png)) {
    logo_png
  } else if (nzchar(logo_gif)) {
    logo_gif
  } else {
    NA_character_
  }

  # -------------------------------------------------------
  # 5. FLAGS DE CONTROLE
  # -------------------------------------------------------
  allow_lgbt_tables <- sogi.filter %in% c("all", "lgbt")

  # -------------------------------------------------------
  # 6. ESTRUTURA DO REPORT
  # -------------------------------------------------------
  report <- list(
    data = list(raw = df),

    tables = list(
      overview = NULL,
      t1 = NULL,
      t2 = NULL,
      t3 = NULL,
      t4 = NULL
    ),

    graphs = list(
      g_overview = NULL,
      g1 = NULL,
      g2 = NULL,
      g3 = NULL,
      g4 = NULL,
      g5 = NULL
    ),

    text = list(
      overview = NULL,
      t1 = NULL,
      t2 = NULL,
      t3 = NULL,
      t4 = NULL,
      g1 = NULL,
      g2 = NULL,
      g3 = NULL,
      g4 = NULL,
      g5 = NULL
    ),

    log = list(),
    steps = character(),

    meta = list(
      idioma = idioma,
      sogi.filter = sogi.filter,
      allow_lgbt_tables = allow_lgbt_tables,
      var.by = var.by,
      add.p = isTRUE(add.p),
      time.comp = time.comp,
      graph.comp = isTRUE(graph.comp),
      logo_path = logo_path,
      created_at = Sys.time()
    )
  )

  class(report) <- "dataLGBT_report"

  return(report)
}
