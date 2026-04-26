# =========================
# CORE: REPORT
# =========================

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

  # -------------------------------------------------------
  # 1. EXTRACAO
  # -------------------------------------------------------
  if (!is.null(proc_data$data$final) && is.data.frame(proc_data$data$final)) {
    df <- proc_data$data$final
  } else if (!is.null(proc_data$data) && is.data.frame(proc_data$data)) {
    df <- proc_data$data
  } else {
    stop("`proc_data$data` não foi encontrado ou é inválido.", call. = FALSE)
  }

  # -------------------------------------------------------
  # 2. DATA QUALITY LAYER
  # -------------------------------------------------------
  quality_df <- data.frame(
    variavel = names(df),
    tipo = vapply(df, function(x) class(x)[1], character(1)),
    n_na = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    pct_na = vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)),
    n_unique = vapply(df, function(x) length(unique(x)), numeric(1)),
    stringsAsFactors = FALSE
  )

  report_quality <- list(
    summary = list(
      n_rows = nrow(df),
      n_cols = ncol(df),
      total_na = sum(is.na(df)),
      pct_complete = round(mean(complete.cases(df)) * 100, 2)
    ),
    table = quality_df,
    alerts = list(
      high_na = quality_df$variavel[quality_df$pct_na > 30]
    )
  )

  # -------------------------------------------------------
  # 3. SANITIZAÇÃO RAW (uso interno)
  # -------------------------------------------------------
  sanitize_chr <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "Ignorado"
    x
  }

  df <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.character), sanitize_chr)
    )

  # datas críticas (evita warning no Quarto)
  if ("Data_Cadastro" %in% names(df)) {
    df$Data_Cadastro <- as.Date(df$Data_Cadastro)
  }

  if ("Raca_Cor" %in% names(df)) {
    df$Raca_Cor <- sanitize_chr(df$Raca_Cor)
  }

  if ("SGM" %in% names(df)) {
    df$SGM <- sanitize_chr(df$SGM)
  }

  if ("Morte" %in% names(df)) {
    df$Morte <- as.character(df$Morte)
  }

  # -------------------------------------------------------
  # 4. RENDER LAYER (CRÍTICO PARA QUARTO)
  # -------------------------------------------------------
  df_render <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.character), ~ tidyr::replace_na(.x, "Ignorado"))
    )

  # -------------------------------------------------------
  # 5. REPORT OBJECT
  # -------------------------------------------------------
  report <- list(

    data = list(
      raw = df,
      render = df_render,
      quality = report_quality
    ),

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
      var.by = var.by,
      add.p = isTRUE(add.p),
      time.comp = time.comp,
      graph.comp = isTRUE(graph.comp),
      created_at = Sys.time()
    )
  )

  class(report) <- "dataLGBT_report"
  return(report)
}
