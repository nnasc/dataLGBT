# =========================
# STEP: REPORT GRAPH 2 (CASOS NO TEMPO)
# =========================

step_report_graph2 <- function() {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma   <- report$meta$idioma %||% "pt"
    time.comp <- report$meta$time.comp %||% "ano"

    # -------------------------------------------------------
    # 1. Definir variável de data
    # -------------------------------------------------------
    var_data <- NULL

    if ("Data_Violencia" %in% names(df)) {
      var_data <- "Data_Violencia"
    } else if ("Data_Obito" %in% names(df)) {
      var_data <- "Data_Obito"
    } else {

      warning("Nenhuma variável de data encontrada.")

      report$graphs$g2 <- NULL
      report$text$g2 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Construir série temporal
    # -------------------------------------------------------
    ts <- .dataLGBT_build_time_series(
      df = df,
      var_data = var_data,
      filtro = NULL,
      time.comp = time.comp,
      idioma = idioma
    )

    if (is.null(ts)) {

      report$graphs$g2 <- NULL
      report$text$g2 <- "Dados insuficientes para gráfico de casos."

      return(report)
    }

    df_plot <- ts$data

    # -------------------------------------------------------
    # 3. Gráfico
    # -------------------------------------------------------
    g2 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = tempo, y = n, group = 1)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = ifelse(idioma == "pt",
                       "Distribuição de Casos ao Longo do Tempo",
                       "Distribution of Cases Over Time"),
        x = ts$x_lab,
        y = ifelse(idioma == "pt", "Número de Casos", "Number of Cases")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    # -------------------------------------------------------
    # 4. Texto interpretativo
    # -------------------------------------------------------
    pico <- df_plot$tempo[which.max(df_plot$n)]
    max_val <- max(df_plot$n)

    if (idioma == "pt") {

      texto2 <- paste0(
        "O Gráfico 2 apresenta a distribuição dos casos ao longo do tempo. ",
        "Observa-se um pico em ", pico, ", com ", max_val, " casos registrados."
      )

    } else {

      texto2 <- paste0(
        "Graph 2 shows the distribution of cases over time. ",
        "A peak was observed in ", pico, " with ", max_val, " cases."
      )
    }

    # -------------------------------------------------------
    # 5. Salvar
    # -------------------------------------------------------
    report$graphs$g2 <- g2
    report$text$g2 <- texto2

    report$steps <- c(report$steps, "graph2")

    report$log$graph2 <- list(
      timestamp = Sys.time(),
      n_points = nrow(df_plot),
      time_unit = time.comp
    )

    return(report)
  }
}
