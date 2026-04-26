# =========================
# STEP: REPORT TABLE 3 (DALY)
# =========================

step_report_table3 <- function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma <- report$meta$idioma %||% "pt"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Checagem de variáveis essenciais
    # -------------------------------------------------------
    required_vars <- c("APVP", "AVCI", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para cálculo de DALY não encontradas.")

      report$tables$t3 <- NULL
      report$text$t3 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Preparar variável SGM
    # -------------------------------------------------------
    use_sgm <- sogi.filter %in% c("all", "lgbt") && "SGM" %in% names(df)

    if (use_sgm) {
      df$SGM <- as.character(df$SGM)
      df$SGM[is.na(df$SGM) | trimws(df$SGM) == ""] <- "Ignorado"
    } else {
      df$SGM <- "Total"
    }

    # -------------------------------------------------------
    # 3. Garantir tipos numéricos
    # -------------------------------------------------------
    df$APVP <- suppressWarnings(as.numeric(df$APVP))
    df$AVCI <- suppressWarnings(as.numeric(df$AVCI))

    # -------------------------------------------------------
    # 4. Calcular componentes DALY
    # -------------------------------------------------------
    tabela3 <- df %>%
      dplyr::group_by(SGM) %>%
      dplyr::summarise(
        Casos  = dplyr::n(),
        Obitos = sum(Morte == "1 - Sim", na.rm = TRUE),

        # YLL (Years of Life Lost)
        YLL = sum(APVP, na.rm = TRUE),

        # YLD (Years Lived with Disability) — já ponderado no proc
        YLD = sum(AVCI, na.rm = TRUE),

        # DALY
        DALY = YLL + YLD,

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        YLL  = round(YLL, 2),
        YLD  = round(YLD, 2),
        DALY = round(DALY, 2)
      ) %>%
      dplyr::arrange(dplyr::desc(DALY))

    # -------------------------------------------------------
    # 5. Texto interpretativo
    # -------------------------------------------------------
    total_daly <- sum(tabela3$DALY, na.rm = TRUE)

    if (nrow(tabela3) > 0 && any(!is.na(tabela3$DALY))) {
      grupo_max <- tabela3$SGM[which.max(tabela3$DALY)]
      max_daly  <- max(tabela3$DALY, na.rm = TRUE)
    } else {
      grupo_max <- NA_character_
      max_daly  <- NA_real_
    }

    if (idioma == "pt") {

      texto3 <- paste0(
        "A Tabela 3 apresenta a carga de doença associada à violência, estimada por DALY (Disability-Adjusted Life Years). ",
        "O total estimado foi de ", round(total_daly, 2), ". ",
        if (!is.na(grupo_max)) {
          paste0(
            "O grupo com maior carga foi ", grupo_max,
            " (DALY = ", round(max_daly, 2), "). "
          )
        } else {
          ""
        },
        "Os DALYs combinam anos de vida perdidos por morte prematura (YLL) ",
        "e anos vividos com incapacidade (YLD)."
      )

    } else {

      texto3 <- paste0(
        "Table 3 presents the burden of disease related to violence, estimated using DALYs (Disability-Adjusted Life Years). ",
        "The total estimated DALYs were ", round(total_daly, 2), ". ",
        if (!is.na(grupo_max)) {
          paste0(
            "The group with the highest burden was ", grupo_max,
            " (DALY = ", round(max_daly, 2), "). "
          )
        } else {
          ""
        },
        "DALYs combine years of life lost (YLL) and years lived with disability (YLD)."
      )
    }

    # -------------------------------------------------------
    # 6. Salvar no objeto
    # -------------------------------------------------------
    report$tables$t3 <- tabela3
    report$text$t3 <- texto3

    report$steps <- c(report$steps, "table3")

    report$log$table3 <- list(
      timestamp = Sys.time(),
      total_daly = total_daly,
      n_rows = nrow(tabela3),
      use_sgm = use_sgm
    )

    return(report)
  }
