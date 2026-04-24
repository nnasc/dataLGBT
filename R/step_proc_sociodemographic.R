# =========================
# STEP: SOCIODEMOGRÁFICO
# =========================

step_proc_sociodemographic <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes.")
    }

    df <- pipe$data$proc$data

    # -------------------------
    # 2. Garantir tipos
    # -------------------------
    df <- dplyr::mutate(
      df,
      dplyr::across(where(is.factor), as.character)
    )

    # -------------------------
    # 3. Construção das variáveis
    # -------------------------
    df <- dplyr::mutate(
      df,

      # ---------------------
      # Sexo
      # ---------------------
      Sexo = dplyr::case_when(
        SEXO.x == "F" ~ "Feminino",
        SEXO.x == "M" ~ "Masculino",
        TRUE ~ NA_character_
      ),

      # ---------------------
      # Idade na violência (SINAN)
      # ---------------------
      Idade_Violencia = suppressWarnings(as.numeric(NU_IDADE_N.x)) - 4000,

      # ---------------------
      # Idade no óbito (SIM)
      # ---------------------
      Idade_Obito = suppressWarnings(as.numeric(IDADE)) - 400,

      # ---------------------
      # Raça/Cor
      # ---------------------
      Raca_Cor = dplyr::case_when(
        RACACOR.x == 1 ~ "Branca",
        RACACOR.x %in% c(2,4) ~ "Preta/Parda",
        RACACOR.x == 3 ~ "Amarela",
        RACACOR.x == 5 ~ "Indigena",
        TRUE ~ NA_character_
      ),

      # ---------------------
      # Escolaridade (Violência)
      # ---------------------
      Escolaridade_Violencia = dplyr::case_when(
        suppressWarnings(as.numeric(CS_ESCOL_N.x)) <= 3 ~ "Ate 8 anos",
        suppressWarnings(as.numeric(CS_ESCOL_N.x)) > 3 ~ "8 anos ou mais",
        TRUE ~ NA_character_
      ),

      # ---------------------
      # Escolaridade (Óbito)
      # ---------------------
      Escolaridade_Obito = dplyr::case_when(
        suppressWarnings(as.numeric(ESC)) <= 3 ~ "Ate 8 anos",
        suppressWarnings(as.numeric(ESC)) > 3 ~ "8 anos ou mais",
        TRUE ~ NA_character_
      ),

      # ---------------------
      # SGM (detalhado)
      # ---------------------
      SGM = dplyr::case_when(

        ORIENT_SEX.x == 1 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) &
          SEXO.x == "F" ~ "0 - HetCisF",

        ORIENT_SEX.x == 1 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) &
          SEXO.x == "M" ~ "0 - HetCisM",

        ORIENT_SEX.x == 2 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) &
          SEXO.x == "F" ~ "1 - Lesbica",

        ORIENT_SEX.x == 2 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) &
          SEXO.x == "M" ~ "1 - Gay",

        ORIENT_SEX.x == 3 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) ~ "3 - Bissexual",

        IDENT_GEN.x == 1 ~ "4 - Travesti",
        IDENT_GEN.x == 2 ~ "5 - Mulher trans",
        IDENT_GEN.x == 3 ~ "6 - Homem trans",

        TRUE ~ NA_character_
      ),

      # ---------------------
      # SGM (binário)
      # ---------------------
      SGM_bin = dplyr::case_when(

        ORIENT_SEX.x == 1 &
          (IDENT_GEN.x %in% c(8,9) | is.na(IDENT_GEN.x)) ~ "0 - HetCis",

        ORIENT_SEX.x %in% c(2,3) ~ "1 - LGBT",

        IDENT_GEN.x %in% c(1,2,3) ~ "1 - LGBT",

        TRUE ~ NA_character_
      ),

      # ---------------------
      # Deficiência
      # ---------------------
      Deficiencia = dplyr::case_when(
        DEF_TRANS.x == 1 ~ "1 - Sim",
        DEF_TRANS.x == 2 ~ "0 - Nao",
        TRUE ~ NA_character_
      )
    )

    # -------------------------
    # 4. Atualizar pipe
    # -------------------------
    pipe$data$proc$data <- df

    pipe$data$proc$steps <- c(
      pipe$data$proc$steps,
      "sociodemografico"
    )

    pipe$proc_meta$steps <- c(
      pipe$proc_meta$steps,
      "sociodemografico"
    )

    return(pipe)
  }
}
