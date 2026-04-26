# =========================
# STEP: REPORT GRAPH 1 (ÓBITOS NO TEMPO)
# =========================

step_report_graph1 <- function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma   <- report$meta$idioma %||% "pt"
    time.comp <- report$meta$time.comp %||% "ano"

    # -------------------------------------------------------
    # 1. Validar variável de morte
    # -------------------------------------------------------
    if (!("Morte" %in% names(df))) {

      warning("Variável 'Morte' não encontrada. Gráfico 1 não será gerado.")

      report$graphs$g1 <- NULL
      report$text$g1 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Selecionar variável de tempo
    # -------------------------------------------------------
    var_data <- NULL

    if ("Data_Obito" %in% names(df)) {
      var_data <- "Data_Obito"
    } else if ("Data_Violencia" %in% names(df)) {
      var_data <- "Data_Violencia"
    } else {

      warning("Nenhuma variável de data encontrada.")

      report$graphs$g1 <- NULL
      report$text$g1 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 3. Filtrar óbitos
    # -------------------------------------------------------
    df <- df %>%
      dplyr::filter(Morte == "1 - Sim")

    if (nrow(df) == 0) {

      report$graphs$g1 <- NULL
      report$text$g1 <- "Não há óbitos para gerar o gráfico."

      return(report)
    }

    # Garantir formato Date
    df[[var_data]] <- as.Date(df[[var_data]])

    df <- df[!is.na(df[[var_data]]), ]

    if (nrow(df) == 0) {

      report$graphs$g1 <- NULL
      report$text$g1 <- "Datas de óbito ausentes."

      return(report)
    }

    # -------------------------------------------------------
    # 4. Criar variável temporal
    # -------------------------------------------------------
    if (time.comp == "ano") {

      df$tempo <- format(df[[var_data]], "%Y")
      x_lab <- ifelse(idioma == "pt", "Ano", "Year")

    } else if (time.comp == "mes") {

      df$tempo <- format(df[[var_data]], "%Y-%m")
      x_lab <- ifelse(idioma == "pt", "Ano-Mês", "Year-Month")

    } else if (time.comp == "dia") {

      df$tempo <- as.character(df[[var_data]])
      x_lab <- ifelse(idioma == "pt", "Data", "Date")

    } else {

      report$graphs$g1 <- NULL
      report$text$g1 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 5. Agregar
    # -------------------------------------------------------
    df_plot <- df %>%
      dplyr::count(tempo, name = "obitos") %>%
      dplyr::arrange(tempo)

    # -------------------------------------------------------
    # 6. Criar gráfico
    # -------------------------------------------------------
    g1 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = tempo, y = obitos, group = 1)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = ifelse(idioma == "pt",
                       "Distribuição de Óbitos ao Longo do Tempo",
                       "Distribution of Deaths Over Time"),
        x = x_lab,
        y = ifelse(idioma == "pt", "Número de Óbitos", "Number of Deaths")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    # -------------------------------------------------------
    # 7. Texto interpretativo
    # -------------------------------------------------------
    pico <- df_plot$tempo[which.max(df_plot$obitos)]
    max_val <- max(df_plot$obitos)

    if (idioma == "pt") {

      texto1 <- paste0(
        "O Gráfico 1 apresenta a distribuição dos óbitos ao longo do tempo. ",
        "Observa-se um pico em ", pico, ", com ", max_val, " óbitos."
      )

    } else {

      texto1 <- paste0(
        "Graph 1 shows the distribution of deaths over time. ",
        "A peak was observed in ", pico, " with ", max_val, " deaths."
      )
    }

    # -------------------------------------------------------
    # 8. Salvar
    # -------------------------------------------------------
    report$graphs$g1 <- g1
    report$text$g1 <- texto1

    report$steps <- c(report$steps, "graph1")

    report$log$graph1 <- list(
      timestamp = Sys.time(),
      n_points = nrow(df_plot),
      time_unit = time.comp
    )

    return(report)
  }
