# =========================
# STEP: REPORT TABLE 4 (CID)
# =========================

step_report_table4 <- function() {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma <- report$meta$idioma %||% "pt"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Verificar variáveis necessárias
    # -------------------------------------------------------
    if (!("Grupo_CID" %in% names(df))) {

      warning("Variável 'Grupo_CID' não encontrada. Tabela 4 não será gerada.")

      report$tables$t4 <- NULL
      report$text$t4 <- NULL

      return(report)
    }

    if (!("Morte" %in% names(df))) {

      warning("Variável 'Morte' não encontrada. Tabela 4 não será gerada.")

      report$tables$t4 <- NULL
      report$text$t4 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Filtrar apenas óbitos
    # -------------------------------------------------------
    df_obito <- df %>%
      dplyr::filter(Morte == "1 - Sim")

    if (nrow(df_obito) == 0) {

      report$tables$t4 <- NULL
      report$text$t4 <- NULL

      report$log$table4 <- list(
        timestamp = Sys.time(),
        skipped = TRUE,
        reason = "Sem óbitos"
      )

      return(report)
    }

    # -------------------------------------------------------
    # 3. Preparar variáveis
    # -------------------------------------------------------
    df_obito$Grupo_CID <- as.character(df_obito$Grupo_CID)
    df_obito$Grupo_CID[is.na(df_obito$Grupo_CID) | trimws(df_obito$Grupo_CID) == ""] <- "Ignorado"

    use_sgm <- sogi.filter %in% c("all", "lgbt") && "SGM" %in% names(df_obito)

    if (use_sgm) {
      df_obito$SGM <- as.character(df_obito$SGM)
      df_obito$SGM[is.na(df_obito$SGM) | trimws(df_obito$SGM) == ""] <- "Ignorado"
    }

    # -------------------------------------------------------
    # 4. Função auxiliar
    # -------------------------------------------------------
    fmt_pct <- function(n, denom) {
      if (denom == 0) return(NA_character_)
      paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
    }

    # -------------------------------------------------------
    # 5. Construir tabela
    # -------------------------------------------------------
    if (use_sgm) {

      total_por_sgm <- df_obito %>%
        dplyr::count(SGM, name = "total_sgm")

      tabela4 <- df_obito %>%
        dplyr::count(Grupo_CID, SGM, name = "n") %>%
        dplyr::left_join(total_por_sgm, by = "SGM") %>%
        dplyr::mutate(valor = fmt_pct(n, total_sgm)) %>%
        dplyr::select(-n, -total_sgm) %>%
        tidyr::pivot_wider(
          names_from = SGM,
          values_from = valor
        )

      # Total geral
      total_geral <- nrow(df_obito)

      total_col <- df_obito %>%
        dplyr::count(Grupo_CID, name = "n_total") %>%
        dplyr::mutate(Total = fmt_pct(n_total, total_geral)) %>%
        dplyr::select(-n_total)

      tabela4 <- dplyr::left_join(total_col, tabela4, by = "Grupo_CID")

    } else {

      total_geral <- nrow(df_obito)

      tabela4 <- df_obito %>%
        dplyr::count(Grupo_CID, name = "n") %>%
        dplyr::mutate(
          Total = fmt_pct(n, total_geral)
        ) %>%
        dplyr::select(-n)
    }

    # Ordenar por frequência
    tabela4 <- tabela4 %>%
      dplyr::arrange(dplyr::desc(Total))

    # -------------------------------------------------------
    # 6. Texto interpretativo
    # -------------------------------------------------------
    principal_causa <- tabela4$Grupo_CID[1]

    if (idioma == "pt") {

      texto4 <- paste0(
        "A Tabela 4 apresenta a distribuição das causas de morte segundo os grupos do CID-10. ",
        "O grupo mais frequente foi ", principal_causa, ". ",
        if (use_sgm) {
          "A distribuição também foi analisada segundo a variável de identidade/orientação sexual (SGM)."
        } else {
          "A análise foi realizada para o conjunto total de óbitos."
        }
      )

    } else {

      texto4 <- paste0(
        "Table 4 presents the distribution of causes of death according to ICD-10 groups. ",
        "The most frequent group was ", principal_causa, ". ",
        if (use_sgm) {
          "The distribution was also stratified by sexual/gender minority (SGM)."
        } else {
          "The analysis was conducted for total deaths."
        }
      )
    }

    # -------------------------------------------------------
    # 7. Salvar
    # -------------------------------------------------------
    report$tables$t4 <- tabela4
    report$text$t4 <- texto4

    report$steps <- c(report$steps, "table4")

    report$log$table4 <- list(
      timestamp = Sys.time(),
      n_rows = nrow(tabela4),
      total_obitos = total_geral,
      use_sgm = use_sgm
    )

    return(report)
  }
}
