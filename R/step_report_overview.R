# =========================
# STEP: REPORT OVERVIEW
# =========================

step_report_overview <- function() {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    if (!is.data.frame(df)) {
      stop("`report$data$raw` deve ser um data.frame.")
    }

    idioma <- report$meta$idioma %||% "pt"
    var.by <- report$meta$var.by %||% NULL

    # -------------------------
    # 1. Indicadores gerais
    # -------------------------
    n_registros <- nrow(df)
    n_variaveis  <- ncol(df)
    n_completos  <- sum(stats::complete.cases(df))
    n_incompletos <- sum(!stats::complete.cases(df))
    pct_completo <- round(mean(stats::complete.cases(df)) * 100, 2)

    n_obitos <- if ("Data_Obito" %in% names(df)) {
      sum(!is.na(df$Data_Obito))
    } else {
      NA_integer_
    }

    n_casos <- if ("Data_Violencia" %in% names(df)) {
      sum(!is.na(df$Data_Violencia))
    } else {
      NA_integer_
    }

    tabela_overview <- data.frame(
      indicador = c(
        "Número de registros",
        "Número de variáveis",
        "Registros completos",
        "Registros incompletos",
        "Percentual de completude",
        "Número de casos com violência",
        "Número de óbitos"
      ),
      valor = c(
        n_registros,
        n_variaveis,
        n_completos,
        n_incompletos,
        pct_completo,
        n_casos,
        n_obitos
      ),
      stringsAsFactors = FALSE
    )

    # -------------------------
    # 2. Texto breve de abertura
    # -------------------------
    if (idioma == "pt") {
      texto_overview <- paste0(
        "Este relatório sintetiza ", n_registros, " registros e ",
        n_variaveis, " variáveis processadas pelo pacote dataLGBT. ",
        "A base apresenta ", pct_completo, "% de completude global. ",
        "A seguir são apresentados os principais achados sobre violência, óbito, ",
        "carga de doença e distribuição temporal dos eventos."
      )
    } else {
      texto_overview <- paste0(
        "This report summarizes ", n_registros, " records and ",
        n_variaveis, " variables processed by the dataLGBT package. ",
        "The dataset has ", pct_completo, "% overall completeness. ",
        "The sections below present the main findings on violence, death, ",
        "burden of disease, and temporal distribution of events."
      )
    }

    # -------------------------
    # 3. Gráfico de abertura
    #    Se var.by existir, mostra distribuição da variável escolhida
    #    Caso contrário, mostra completude por variável (top 15)
    # -------------------------
    grafico_overview <- NULL

    if (!is.null(var.by) && var.by %in% names(df)) {

      tab_g <- as.data.frame(table(df[[var.by]], useNA = "ifany"))
      names(tab_g) <- c("categoria", "n")

      grafico_overview <- ggplot2::ggplot(
        tab_g,
        ggplot2::aes(x = stats::reorder(categoria, n), y = n)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = if (idioma == "pt") "Número de registros" else "Number of records",
          title = if (idioma == "pt") {
            paste0("Distribuição de ", var.by)
          } else {
            paste0("Distribution of ", var.by)
          }
        ) +
        ggplot2::theme_minimal()

    } else {

      miss_df <- data.frame(
        variavel = names(df),
        pct_missing = vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)),
        stringsAsFactors = FALSE
      )

      miss_df <- miss_df[order(miss_df$pct_missing, decreasing = TRUE), , drop = FALSE]
      miss_df <- head(miss_df, 15)

      grafico_overview <- ggplot2::ggplot(
        miss_df,
        ggplot2::aes(x = stats::reorder(variavel, pct_missing), y = pct_missing)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = if (idioma == "pt") "% de ausentes" else "% missing",
          title = if (idioma == "pt") {
            "Principais variáveis com valores ausentes"
          } else {
            "Main variables with missing values"
          }
        ) +
        ggplot2::theme_minimal()
    }

    # -------------------------
    # 4. Salvar no objeto
    # -------------------------
    report$tables$overview <- tabela_overview
    report$text$overview <- texto_overview
    report$graphs$g_overview <- grafico_overview

    report$steps <- c(report$steps, "overview")

    report$log$overview <- list(
      timestamp = Sys.time(),
      n_rows = n_registros,
      n_cols = n_variaveis,
      var.by = var.by,
      pct_complete = pct_completo
    )

    return(report)
  }
}
