# =========================
# STEP: REPORT TABLE 2 (SGM)
# =========================

step_report_table2 <- function() {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma <- report$meta$idioma %||% "pt"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Verificar se deve rodar
    # -------------------------------------------------------
    if (!(sogi.filter %in% c("all", "lgbt"))) {

      report$tables$t2 <- NULL
      report$text$t2 <- NULL

      report$log$table2 <- list(
        timestamp = Sys.time(),
        skipped = TRUE,
        reason = "sogi.filter não permite tabela SGM"
      )

      return(report)
    }

    # -------------------------------------------------------
    # 2. Verificar variáveis necessárias
    # -------------------------------------------------------
    if (!("SGM" %in% names(df))) {

      warning("Variável 'SGM' não encontrada. Tabela 2 não será gerada.")

      report$tables$t2 <- NULL
      report$text$t2 <- NULL

      return(report)
    }

    if (!("Morte" %in% names(df))) {

      warning("Variável 'Morte' não encontrada. Tabela 2 não será gerada.")

      report$tables$t2 <- NULL
      report$text$t2 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 3. Preparar dados
    # -------------------------------------------------------
    df$SGM <- as.character(df$SGM)
    df$SGM[is.na(df$SGM) | trimws(df$SGM) == ""] <- "Ignorado"

    df$Morte <- as.character(df$Morte)

    # -------------------------------------------------------
    # 4. Calcular indicadores
    # -------------------------------------------------------
    tabela2 <- df %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(
        Casos = dplyr::n(),
        Obitos = sum(Morte == "1 - Sim", na.rm = TRUE),
        Letalidade = ifelse(
          Casos > 0,
          round(Obitos / Casos * 100, 2),
          NA_real_
        ),
        .groups = "drop"
      )

    # Ordenar
    tabela2 <- tabela2 %>%
      dplyr::arrange(dplyr::desc(Obitos))

    # -------------------------------------------------------
    # 5. Texto interpretativo simples
    # -------------------------------------------------------
    total_casos <- sum(tabela2$Casos, na.rm = TRUE)
    total_obitos <- sum(tabela2$Obitos, na.rm = TRUE)

    grupo_max <- tabela2$SGM[which.max(tabela2$Letalidade)]
    max_letal <- max(tabela2$Letalidade, na.rm = TRUE)

    if (idioma == "pt") {

      texto2 <- paste0(
        "A Tabela 2 apresenta os indicadores de casos, óbitos e letalidade segundo a variável de identidade/orientação sexual (SGM). ",
        "Foram identificados ", total_casos, " casos e ", total_obitos, " óbitos no total. ",
        "O grupo com maior letalidade foi ", grupo_max, " (", max_letal, "%)."
      )

    } else {

      texto2 <- paste0(
        "Table 2 presents cases, deaths, and case-fatality rates by sexual/gender minority (SGM). ",
        "A total of ", total_casos, " cases and ", total_obitos, " deaths were identified. ",
        "The group with the highest case-fatality rate was ", grupo_max, " (", max_letal, "%)."
      )
    }

    # -------------------------------------------------------
    # 6. Salvar no objeto
    # -------------------------------------------------------
    report$tables$t2 <- tabela2
    report$text$t2 <- texto2

    report$steps <- c(report$steps, "table2")

    report$log$table2 <- list(
      timestamp = Sys.time(),
      n_rows = nrow(tabela2),
      total_casos = total_casos,
      total_obitos = total_obitos
    )

    return(report)
  }
}
