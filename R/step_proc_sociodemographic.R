# =========================
# STEP: SOCIODEMOGRÁFICO
# =========================

step_proc_sociodemo <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (!inherits(pipe, "data_link_pipe") ||
        is.null(pipe$data$proc) ||
        is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes.", call. = FALSE)
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

      Sexo = dplyr::case_when(
        SEXO == "F" ~ "Feminino",
        SEXO == "M" ~ "Masculino",
        TRUE ~ NA_character_
      ),

      Idade_Violencia = suppressWarnings(as.numeric(NU_IDADE_N)) - 4000,
      Idade_Obito = suppressWarnings(as.numeric(IDADE)) - 400,

      Raca_Cor = dplyr::case_when(
        RACACOR == 1 ~ "Branca",
        RACACOR %in% c(2, 4) ~ "Preta/Parda",
        RACACOR == 3 ~ "Amarela",
        RACACOR == 5 ~ "Indigena",
        TRUE ~ NA_character_
      ),

      Escolaridade_Violencia = dplyr::case_when(
        suppressWarnings(as.numeric(CS_ESCOL_N)) <= 3 ~ "Ate 8 anos",
        suppressWarnings(as.numeric(CS_ESCOL_N)) > 3 ~ "8 anos ou mais",
        TRUE ~ NA_character_
      ),

      Escolaridade_Obito = dplyr::case_when(
        suppressWarnings(as.numeric(ESC)) <= 3 ~ "Ate 8 anos",
        suppressWarnings(as.numeric(ESC)) > 3 ~ "8 anos ou mais",
        TRUE ~ NA_character_
      ),

      SGM = dplyr::case_when(
        ORIENT_SEX == 1 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          SEXO == "F" ~ "0 - HetCisF",

        ORIENT_SEX == 1 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          SEXO == "M" ~ "0 - HetCisM",

        ORIENT_SEX == 2 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          SEXO == "F" ~ "1 - Lesbica",

        ORIENT_SEX == 2 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          SEXO == "M" ~ "1 - Gay",

        ORIENT_SEX == 3 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) ~ "3 - Bissexual",

        IDENT_GEN == 1 ~ "4 - Travesti",
        IDENT_GEN == 2 ~ "5 - Mulher trans",
        IDENT_GEN == 3 ~ "6 - Homem trans",

        TRUE ~ NA_character_
      ),

      SGM_bin = dplyr::case_when(
        ORIENT_SEX == 1 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) ~ "0 - HetCis",

        ORIENT_SEX %in% c(2, 3) ~ "1 - LGBT",
        IDENT_GEN %in% c(1, 2, 3) ~ "1 - LGBT",

        TRUE ~ NA_character_
      ),

      Deficiencia = dplyr::case_when(
        DEF_TRANS == 1 ~ "1 - Sim",
        DEF_TRANS == 2 ~ "0 - Nao",
        TRUE ~ NA_character_
      )
    )

    # -------------------------
    # 4. Atualizar pipe
    # -------------------------
    pipe$data$proc$data <- df
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "sociodemografico")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "sociodemografico")

    return(pipe)
  }
}
