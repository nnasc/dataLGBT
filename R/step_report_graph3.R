# =========================
# STEP: REPORT GRAPH 3 (LETALIDADE POR SGM)
# =========================

step_report_graph3 <- function(max_lines = 6) {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma   <- report$meta$idioma %||% "pt"
    time.comp <- report$meta$time.comp %||% "ano"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Checagens
    # -------------------------------------------------------
    if (!(sogi.filter %in% c("all", "lgbt"))) {

      report$graphs$g3 <- NULL
      report$text$g3 <- NULL

      return(report)
    }

    required_vars <- c("SGM", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para graph3 não encontradas.")

      report$graphs$g3 <- NULL
      report$text$g3 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Definir variável de data
    # -------------------------------------------------------
    var_data <- NULL

    if ("Data_Violencia" %in% names(df)) {
      var_data <- "Data_Violencia"
    } else if ("Data_Obito" %in% names(df)) {
      var_data <- "Data_Obito"
    } else {

      report$graphs$g3 <- NULL
      report$text$g3 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 3. Preparar dados
    # -------------------------------------------------------
    df$SGM <- as.character(df$SGM)
    df$SGM[is.na(df$SGM) | trimws(df$SGM) == ""] <- "Ignorado"

    df[[var_data]] <- as.Date(df[[var_data]])
    df <- df[!is.na(df[[var_data]]), ]

    if (nrow(df) == 0) {
      report$graphs$g3 <- NULL
      report$text$g3 <- NULL
      return(report)
    }

    # -------------------------------------------------------
    # 4. Criar tempo
    # -------------------------------------------------------
    if (time.comp == "ano") {
      df$tempo <- format(df[[var_data]], "%Y")
      x_lab <- ifelse(idioma == "pt", "Ano", "Year")

    } else if (time.comp == "mes") {
      df$tempo <- format(df[[var_data]], "%Y-%m")
      x_lab <- ifelse(idioma == "pt", "Ano-Mês", "Year-Month")

    } else {
      report$graphs$g3 <- NULL
      report$text$g3 <- NULL
      return(report)
    }

    # -------------------------------------------------------
    # 5. Calcular letalidade
    # -------------------------------------------------------
    df_plot <- df %>%
      dplyr::group_by(tempo, SGM) %>%
      dplyr::summarise(
        casos = dplyr::n(),
        obitos = sum(Morte == "1 - Sim", na.rm = TRUE),
        letalidade = ifelse(casos > 0, obitos / casos * 100, NA_real_),
        .groups = "drop"
      )

    # -------------------------------------------------------
    # 6. Reduzir número de grupos (se necessário)
    # -------------------------------------------------------
    grupos_top <- df_plot %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(total = sum(casos), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::slice_head(n = max_lines) %>%
      dplyr::pull(SGM)

    df_plot$SGM_plot <- ifelse(
      df_plot$SGM %in% grupos_top,
      df_plot$SGM,
      "Outros"
    )

    # -------------------------------------------------------
    # 7. Escolher tipo de gráfico
    # -------------------------------------------------------
    n_grupos <- length(unique(df_plot$SGM_plot))

    if (n_grupos <= max_lines) {

      # MULTILINE
      g3 <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = tempo, y = letalidade, color = SGM_plot, group = SGM_plot)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = ifelse(idioma == "pt",
                         "Letalidade ao Longo do Tempo por SGM",
                         "Case-Fatality Over Time by SGM"),
          x = x_lab,
          y = ifelse(idioma == "pt", "Letalidade (%)", "Case-Fatality (%)"),
          color = "SGM"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

    } else {

      # FACET fallback
      g3 <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = tempo, y = letalidade)
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~SGM_plot) +
        ggplot2::labs(
          title = ifelse(idioma == "pt",
                         "Letalidade ao Longo do Tempo por SGM",
                         "Case-Fatality Over Time by SGM"),
          x = x_lab,
          y = ifelse(idioma == "pt", "Letalidade (%)", "Case-Fatality (%)")
        ) +
        ggplot2::theme_minimal()
    }

    # -------------------------------------------------------
    # 8. Texto
    # -------------------------------------------------------
    grupo_max <- df_plot$SGM[which.max(df_plot$letalidade)]

    if (idioma == "pt") {
      texto3 <- paste0(
        "O Gráfico 3 apresenta a evolução da letalidade ao longo do tempo. ",
        "O grupo com maior letalidade observada foi ", grupo_max, "."
      )
    } else {
      texto3 <- paste0(
        "Graph 3 shows the evolution of case-fatality over time. ",
        "The group with the highest observed case-fatality was ", grupo_max, "."
      )
    }

    # -------------------------------------------------------
    # 9. Salvar
    # -------------------------------------------------------
    report$graphs$g3 <- g3
    report$text$g3 <- texto3

    report$steps <- c(report$steps, "graph3")

    report$log$graph3 <- list(
      timestamp = Sys.time(),
      n_groups = n_grupos,
      time_unit = time.comp
    )

    return(report)
  }
}
