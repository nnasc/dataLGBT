# =========================
# STEP: REPORT GRAPH 3 (LETALIDADE POR SGM)
# =========================

step_report_graph3 <- function(report) {

  if (!inherits(report, "dataLGBT_report")) {
    stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
  }

  max_lines <- report$meta$graph3_max_lines %||% 6
  min_n     <- report$meta$graph3_min_n %||% 5

  df <- report$data$raw

    idioma      <- report$meta$idioma %||% "pt"
    time.comp   <- report$meta$time.comp %||% "ano"
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
    # 2. Variável de data
    # -------------------------------------------------------
    var_data <- if ("Data_Violencia" %in% names(df)) {
      "Data_Violencia"
    } else if ("Data_Obito" %in% names(df)) {
      "Data_Obito"
    } else {
      report$graphs$g3 <- NULL
      report$text$g3 <- NULL
      return(report)
    }

    # -------------------------------------------------------
    # 3. Preparar dados
    # -------------------------------------------------------
    df <- df %>%
      dplyr::mutate(
        SGM = as.character(SGM),
        SGM = dplyr::if_else(is.na(SGM) | trimws(SGM) == "", "Ignorado", SGM),
        Morte = as.character(Morte),
        data_ref = as.Date(.data[[var_data]])
      ) %>%
      dplyr::filter(!is.na(data_ref))

    if (nrow(df) == 0) {
      report$graphs$g3 <- NULL
      report$text$g3 <- NULL
      return(report)
    }

    # -------------------------------------------------------
    # 4. Criar eixo temporal
    # -------------------------------------------------------
    if (time.comp == "ano") {
      df$tempo <- format(df$data_ref, "%Y")
      x_lab <- ifelse(idioma == "pt", "Ano", "Year")

    } else if (time.comp == "mes") {
      df$tempo <- format(df$data_ref, "%Y-%m")
      x_lab <- ifelse(idioma == "pt", "Ano-Mês", "Year-Month")

    } else {
      report$graphs$g3 <- NULL
      report$text$g3 <- NULL
      return(report)
    }

    # -------------------------------------------------------
    # 5. Agregar dados
    # -------------------------------------------------------
    df_plot <- df %>%
      dplyr::group_by(tempo, SGM) %>%
      dplyr::summarise(
        casos = dplyr::n(),
        obitos = sum(Morte == "1 - Sim", na.rm = TRUE),
        .groups = "drop"
      )

    # -------------------------------------------------------
    # 6. Filtrar baixa estabilidade
    # -------------------------------------------------------
    df_plot <- df_plot %>%
      dplyr::mutate(
        letalidade = ifelse(casos >= min_n, obitos / casos * 100, NA_real_)
      )

    # -------------------------------------------------------
    # 7. Selecionar grupos principais
    # -------------------------------------------------------
    grupos_top <- df_plot %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(total = sum(casos), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::slice_head(n = max_lines) %>%
      dplyr::pull(SGM)

    df_plot$SGM_plot <- ifelse(df_plot$SGM %in% grupos_top, df_plot$SGM, "Outros")

    # -------------------------------------------------------
    # 8. REAGREGAR após colapso
    # -------------------------------------------------------
    df_plot <- df_plot %>%
      dplyr::group_by(tempo, SGM_plot) %>%
      dplyr::summarise(
        casos = sum(casos),
        obitos = sum(obitos),
        letalidade = ifelse(casos >= min_n, obitos / casos * 100, NA_real_),
        .groups = "drop"
      )

    # -------------------------------------------------------
    # 9. Gráfico
    # -------------------------------------------------------
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

    # -------------------------------------------------------
    # 10. Texto (ponderado)
    # -------------------------------------------------------
    resumo <- df_plot %>%
      dplyr::group_by(SGM_plot) %>%
      dplyr::summarise(
        letal_media = sum(obitos) / sum(casos),
        .groups = "drop"
      )

    grupo_max <- resumo$SGM_plot[which.max(resumo$letal_media)]

    if (idioma == "pt") {

      texto3 <- paste0(
        "O Gráfico 3 apresenta a evolução da letalidade ao longo do tempo por SGM. ",
        "Foram considerados apenas períodos com número mínimo de casos para estabilidade das estimativas. ",
        "O grupo com maior letalidade média foi ", grupo_max, "."
      )

    } else {

      texto3 <- paste0(
        "Graph 3 shows the evolution of case-fatality over time by SGM. ",
        "Only periods with a minimum number of cases were considered for stability. ",
        "The group with the highest average case-fatality was ", grupo_max, "."
      )
    }

    # -------------------------------------------------------
    # 11. Salvar
    # -------------------------------------------------------
    report$graphs$g3 <- g3
    report$text$g3 <- texto3

    report$steps <- c(report$steps, "graph3")

    report$log$graph3 <- list(
      timestamp = Sys.time(),
      n_groups = length(unique(df_plot$SGM_plot)),
      min_n = min_n,
      time_unit = time.comp
    )

    return(report)
  }
