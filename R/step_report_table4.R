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
    # 1. Checagem de variáveis
    # -------------------------------------------------------
    required_vars <- c("Grupo_CID", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para Tabela 4 não encontradas.")

      report$tables$t4 <- NULL
      report$text$t4 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Filtrar óbitos
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
    # 3. Limpeza de variáveis
    # -------------------------------------------------------
    df_obito <- df_obito %>%
      dplyr::mutate(
        Grupo_CID = as.character(Grupo_CID),
        Grupo_CID = dplyr::if_else(
          is.na(Grupo_CID) | trimws(Grupo_CID) == "",
          "Ignorado",
          Grupo_CID
        )
      )

    # -------------------------------------------------------
    # 4. Definir uso de SGM
    # -------------------------------------------------------
    use_sgm <- sogi.filter %in% c("all", "lgbt") && "SGM" %in% names(df_obito)

    if (use_sgm) {
      df_obito$SGM <- as.character(df_obito$SGM)
      df_obito$SGM[is.na(df_obito$SGM) | trimws(df_obito$SGM) == ""] <- "Ignorado"
    }

    # -------------------------------------------------------
    # 5. Função percentual
    # -------------------------------------------------------
    fmt_pct <- function(n, denom) {
      if (is.na(denom) || denom == 0) return(NA_character_)
      paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
    }

    total_geral <- nrow(df_obito)

    # -------------------------------------------------------
    # 6. Construção da tabela
    # -------------------------------------------------------
    if (use_sgm) {

      # Total por SGM
      total_por_sgm <- df_obito %>%
        dplyr::count(SGM, name = "total_sgm")

      # Tabela estratificada
      tabela4 <- df_obito %>%
        dplyr::count(Grupo_CID, SGM, name = "n") %>%
        dplyr::left_join(total_por_sgm, by = "SGM") %>%
        dplyr::mutate(valor = fmt_pct(n, total_sgm)) %>%
        dplyr::select(Grupo_CID, SGM, valor) %>%
        tidyr::pivot_wider(
          names_from = SGM,
          values_from = valor,
          values_fill = NA_character_
        )

      # Total geral por causa (USADO PARA ORDENAR)
      total_col <- df_obito %>%
        dplyr::count(Grupo_CID, name = "n_total") %>%
        dplyr::mutate(
          Total = fmt_pct(n_total, total_geral)
        )

      tabela4 <- total_col %>%
        dplyr::left_join(tabela4, by = "Grupo_CID") %>%
        dplyr::arrange(dplyr::desc(n_total)) %>%
        dplyr::select(-n_total)

    } else {

      tabela4 <- df_obito %>%
        dplyr::count(Grupo_CID, name = "n_total") %>%
        dplyr::mutate(
          Total = fmt_pct(n_total, total_geral)
        ) %>%
        dplyr::arrange(dplyr::desc(n_total)) %>%
        dplyr::select(-n_total)
    }

    # -------------------------------------------------------
    # 7. Texto interpretativo
    # -------------------------------------------------------
    principal_causa <- tabela4$Grupo_CID[1]

    if (idioma == "pt") {

      texto4 <- paste0(
        "A Tabela 4 apresenta a distribuição das causas de morte segundo os grupos do CID-10. ",
        if (!is.na(principal_causa)) {
          paste0("O grupo mais frequente foi ", principal_causa, ". ")
        } else {
          ""
        },
        if (use_sgm) {
          "A análise foi estratificada por SGM."
        } else {
          "A análise refere-se ao total de óbitos."
        }
      )

    } else {

      texto4 <- paste0(
        "Table 4 presents the distribution of causes of death according to ICD-10 groups. ",
        if (!is.na(principal_causa)) {
          paste0("The most frequent group was ", principal_causa, ". ")
        } else {
          ""
        },
        if (use_sgm) {
          "The analysis was stratified by SGM."
        } else {
          "The analysis refers to total deaths."
        }
      )
    }

    # -------------------------------------------------------
    # 8. Salvar
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
