# =========================
# STEP: REPORT TABLE 3 (DALY)
# =========================

step_report_table3 <- function(dw_fisica = 0.165,
                               dw_psico  = 0.133) {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma <- report$meta$idioma %||% "pt"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Verificar variáveis essenciais
    # -------------------------------------------------------
    required_vars <- c("APVP", "AVCI", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para cálculo de DALY não encontradas.")

      report$tables$t3 <- NULL
      report$text$t3 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Criar variável SGM (se aplicável)
    # -------------------------------------------------------
    use_sgm <- sogi.filter %in% c("all", "lgbt") && "SGM" %in% names(df)

    if (use_sgm) {
      df$SGM <- as.character(df$SGM)
      df$SGM[is.na(df$SGM) | trimws(df$SGM) == ""] <- "Ignorado"
    } else {
      df$SGM <- "Total"
    }

    # -------------------------------------------------------
    # 3. Criar proxy de tipo de violência
    # -------------------------------------------------------
    if ("Violencia_Fisica_Ampla" %in% names(df)) {
      df$tipo_viol <- ifelse(df$Violencia_Fisica_Ampla == "1 - Sim", "fisica", "psico")
    } else {
      df$tipo_viol <- "psico"
    }

    # -------------------------------------------------------
    # 4. Garantir tipos numéricos
    # -------------------------------------------------------
    df$APVP <- suppressWarnings(as.numeric(df$APVP))
    df$AVCI <- suppressWarnings(as.numeric(df$AVCI))

    # -------------------------------------------------------
    # 5. Calcular componentes
    # -------------------------------------------------------
    tabela3 <- df %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(
        Casos = dplyr::n(),
        Obitos = sum(Morte == "1 - Sim", na.rm = TRUE),

        # YLL = soma do APVP
        YLL = sum(APVP, na.rm = TRUE),

        # YLD ponderado
        YLD_fis = sum(
          AVCI[tipo_viol == "fisica"] * dw_fisica,
          na.rm = TRUE
        ),
        YLD_psico = sum(
          AVCI[tipo_viol == "psico"] * dw_psico,
          na.rm = TRUE
        ),

        YLD = YLD_fis + YLD_psico,

        DALY = YLL + YLD,

        .groups = "drop"
      )

    # -------------------------------------------------------
    # 6. Arredondar
    # -------------------------------------------------------
    tabela3 <- tabela3 %>%
      dplyr::mutate(
        YLL  = round(YLL, 2),
        YLD  = round(YLD, 2),
        DALY = round(DALY, 2)
      ) %>%
      dplyr::arrange(dplyr::desc(DALY))

    # -------------------------------------------------------
    # 7. Texto interpretativo
    # -------------------------------------------------------
    total_daly <- sum(tabela3$DALY, na.rm = TRUE)
    grupo_max <- tabela3$SGM[which.max(tabela3$DALY)]
    max_daly <- max(tabela3$DALY, na.rm = TRUE)

    if (idioma == "pt") {

      texto3 <- paste0(
        "A Tabela 3 apresenta as medidas de carga de doença associadas à violência. ",
        "O total de DALYs estimado foi de ", round(total_daly, 2), ". ",
        "O grupo com maior carga de doença foi ", grupo_max,
        " (DALY = ", round(max_daly, 2), "). ",
        "Os anos de vida perdidos (APVP/YLL) e os anos vividos com incapacidade (AVCI/YLD) ",
        "foram combinados para estimar o impacto total da violência."
      )

    } else {

      texto3 <- paste0(
        "Table 3 presents the burden of disease measures related to violence. ",
        "Total DALYs were estimated at ", round(total_daly, 2), ". ",
        "The group with the highest burden was ", grupo_max,
        " (DALY = ", round(max_daly, 2), "). ",
        "Years of life lost (YLL) and years lived with disability (YLD) ",
        "were combined to estimate the total impact."
      )
    }

    # -------------------------------------------------------
    # 8. Salvar
    # -------------------------------------------------------
    report$tables$t3 <- tabela3
    report$text$t3 <- texto3

    report$steps <- c(report$steps, "table3")

    report$log$table3 <- list(
      timestamp = Sys.time(),
      total_daly = total_daly,
      n_rows = nrow(tabela3),
      dw_fisica = dw_fisica,
      dw_psico = dw_psico
    )

    return(report)
  }
}
