# =========================
# STEP: REPORT TABLE 2 (SGM)
# =========================

step_report_table2 <- function(report) {

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
    required_vars <- c("SGM", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para Tabela 2 não encontradas.")

      report$tables$t2 <- NULL
      report$text$t2 <- NULL

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
        Morte = dplyr::if_else(is.na(Morte), "0 - Nao", Morte)
      )

    # -------------------------------------------------------
    # 4. Calcular indicadores
    # -------------------------------------------------------
    tabela2 <- df %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(
        Casos = dplyr::n(),
        Obitos = sum(Morte == "1 - Sim", na.rm = TRUE),
        Letalidade = dplyr::if_else(
          Casos > 0,
          round(Obitos / Casos * 100, 2),
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Obitos))

    # -------------------------------------------------------
    # 5. Texto interpretativo
    # -------------------------------------------------------
    total_casos <- sum(tabela2$Casos, na.rm = TRUE)
    total_obitos <- sum(tabela2$Obitos, na.rm = TRUE)

    if (nrow(tabela2) > 0 && any(!is.na(tabela2$Letalidade))) {
      grupo_max <- tabela2$SGM[which.max(tabela2$Letalidade)]
      max_letal <- max(tabela2$Letalidade, na.rm = TRUE)
    } else {
      grupo_max <- NA_character_
      max_letal <- NA_real_
    }

    if (idioma == "pt") {

      texto2 <- paste0(
        "A Tabela 2 apresenta os indicadores de casos, óbitos e letalidade segundo a variável SGM. ",
        "Foram identificados ", total_casos, " casos e ", total_obitos, " óbitos no total. ",
        if (!is.na(grupo_max)) {
          paste0("O grupo com maior letalidade foi ", grupo_max, " (", max_letal, "%).")
        } else {
          "Não foi possível identificar o grupo com maior letalidade."
        }
      )

    } else {

      texto2 <- paste0(
        "Table 2 presents cases, deaths, and case-fatality rates by SGM. ",
        "A total of ", total_casos, " cases and ", total_obitos, " deaths were identified. ",
        if (!is.na(grupo_max)) {
          paste0("The group with the highest case-fatality rate was ", grupo_max, " (", max_letal, "%).")
        } else {
          "It was not possible to identify the group with the highest case-fatality rate."
        }
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
