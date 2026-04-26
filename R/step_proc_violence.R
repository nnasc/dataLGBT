# =========================
# STEP: VIOLÊNCIA
# =========================

step_proc_violence <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (!inherits(pipe, "data_link_pipe") ||
        is.null(pipe$data$proc) ||
        is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_violence()`.", call. = FALSE)
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
    # 3. Variáveis da violência
    # -------------------------
    df <- dplyr::mutate(
      df,

      # Data da violência
      Data_Violencia = dplyr::case_when(
        !is.na(DT_OCOR) ~ DT_OCOR,
        is.na(DT_OCOR) & !is.na(DT_NOTIFIC) ~ DT_NOTIFIC,
        TRUE ~ NA
      ),

      # Violência em repetição
      Violencia_Repetida = dplyr::case_when(
        OUT_VEZES == 1 ~ "1 - Sim",
        OUT_VEZES == 2 ~ "0 - Nao",
        OUT_VEZES == 9 ~ NA_character_,
        TRUE ~ NA_character_
      ),

      # Lesão autoprovocada
      Lesao_Autoprovocada = dplyr::case_when(
        LES_AUTOP == 1 ~ "1 - Sim",
        LES_AUTOP == 2 ~ "0 - Nao",
        LES_AUTOP == 8 ~ "0 - Nao",
        LES_AUTOP == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      # Violência motivada por
      Violencia_Motivada = dplyr::case_when(
        VIOL_MOTIV == 1 ~ "Sexismo",
        VIOL_MOTIV == 2 ~ "Homofobia/Lesbofobia/Bifobia/Transfobia",
        VIOL_MOTIV == 3 ~ "Racismo",
        VIOL_MOTIV == 4 ~ "Intolerancia religiosa",
        VIOL_MOTIV == 5 ~ "Xenofobia",
        VIOL_MOTIV == 6 ~ "Conflito geracional",
        VIOL_MOTIV == 7 ~ "Situacao de rua",
        VIOL_MOTIV == 8 ~ "Deficiencia",
        VIOL_MOTIV == 9 ~ NA_character_,
        VIOL_MOTIV == 88 ~ NA_character_,
        VIOL_MOTIV == 99 ~ NA_character_,
        TRUE ~ NA_character_
      ),

      # Tipo de violência
      Violencia_Fisica = dplyr::case_when(
        VIOL_FISIC == 1 ~ "1 - Sim",
        VIOL_FISIC == 2 ~ "0 - Nao",
        VIOL_FISIC == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Psico_Moral = dplyr::case_when(
        VIOL_PSICO == 1 ~ "1 - Sim",
        VIOL_PSICO == 2 ~ "0 - Nao",
        VIOL_PSICO == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Tortura = dplyr::case_when(
        VIOL_TORT == 1 ~ "1 - Sim",
        VIOL_TORT == 2 ~ "0 - Nao",
        VIOL_TORT == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Sexual = dplyr::case_when(
        VIOL_SEXU == 1 ~ "1 - Sim",
        VIOL_SEXU == 2 ~ "0 - Nao",
        VIOL_SEXU == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Financeira = dplyr::case_when(
        VIOL_FINAN == 1 ~ "1 - Sim",
        VIOL_FINAN == 2 ~ "0 - Nao",
        VIOL_FINAN == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Negligencia_Abandono = dplyr::case_when(
        VIOL_NEGLI == 1 ~ "1 - Sim",
        VIOL_NEGLI == 2 ~ "0 - Nao",
        VIOL_NEGLI == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Trabalho_Infantil = dplyr::case_when(
        VIOL_INFAN == 1 ~ "1 - Sim",
        VIOL_INFAN == 2 ~ "0 - Nao",
        VIOL_INFAN == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Intervencao_Legal = dplyr::case_when(
        VIOL_LEGAL == 1 ~ "1 - Sim",
        VIOL_LEGAL == 2 ~ "0 - Nao",
        VIOL_LEGAL == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Tipo_Violencia = dplyr::case_when(
        VIOL_OUTR == 1 ~ "1 - Sim",
        VIOL_OUTR == 2 ~ "0 - Nao",
        VIOL_OUTR == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Tipo_Violencia_Especificar = dplyr::if_else(
        VIOL_OUTR == 1,
        as.character(VIOL_ESPEC),
        NA_character_
      ),

      Violencia_Fisica_Ampla = dplyr::case_when(
        Violencia_Fisica == "1 - Sim" |
          Violencia_Sexual == "1 - Sim" |
          Tortura == "1 - Sim" |
          Negligencia_Abandono == "1 - Sim" |
          Trabalho_Infantil == "1 - Sim" |
          Intervencao_Legal == "1 - Sim" ~ "1 - Sim",
          TRUE ~ "0 - Nao"
      ),

      # Violência sexual - subtipos
      Assedio_Sexual = dplyr::case_when(
        SEX_ASSEDI == 1 ~ "1 - Sim",
        SEX_ASSEDI == 2 ~ "0 - Nao",
        SEX_ASSEDI == 8 ~ "0 - Nao",
        SEX_ASSEDI == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Estupro = dplyr::case_when(
        SEX_ESTUPR == 1 ~ "1 - Sim",
        SEX_ESTUPR == 2 ~ "0 - Nao",
        SEX_ESTUPR == 8 ~ "0 - Nao",
        SEX_ESTUPR == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Pornografia_Infantil = dplyr::case_when(
        SEX_PORNO == 1 ~ "1 - Sim",
        SEX_PORNO == 2 ~ "0 - Nao",
        SEX_PORNO == 8 ~ "0 - Nao",
        SEX_PORNO == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Exploracao_Sexual = dplyr::case_when(
        SEX_EXPLO == 1 ~ "1 - Sim",
        SEX_EXPLO == 2 ~ "0 - Nao",
        SEX_EXPLO == 8 ~ "0 - Nao",
        SEX_EXPLO == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Sexual = dplyr::case_when(
        SEX_OUTRO == 1 ~ "1 - Sim",
        SEX_OUTRO == 2 ~ "0 - Nao",
        SEX_OUTRO == 8 ~ "0 - Nao",
        SEX_OUTRO == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Sexual_Especificar = dplyr::if_else(
        SEX_OUTRO == 1,
        as.character(SEX_ESPEC),
        NA_character_
      ),

      # Procedimentos pós-violência sexual
      Profilaxia_DST = dplyr::case_when(
        PROC_DST == 1 ~ "1 - Sim",
        PROC_DST == 2 ~ "0 - Nao",
        PROC_DST == 8 ~ "0 - Nao",
        PROC_DST == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Profilaxia_HIV = dplyr::case_when(
        PROC_HIV == 1 ~ "1 - Sim",
        PROC_HIV == 2 ~ "0 - Nao",
        PROC_HIV == 8 ~ "0 - Nao",
        PROC_HIV == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Profilaxia_Hepatite_B = dplyr::case_when(
        PROC_HEPB == 1 ~ "1 - Sim",
        PROC_HEPB == 2 ~ "0 - Nao",
        PROC_HEPB == 8 ~ "0 - Nao",
        PROC_HEPB == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Sangue = dplyr::case_when(
        PROC_SANG == 1 ~ "1 - Sim",
        PROC_SANG == 2 ~ "0 - Nao",
        PROC_SANG == 8 ~ "0 - Nao",
        PROC_SANG == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Semen = dplyr::case_when(
        PROC_SEMEN == 1 ~ "1 - Sim",
        PROC_SEMEN == 2 ~ "0 - Nao",
        PROC_SEMEN == 8 ~ "0 - Nao",
        PROC_SEMEN == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Secrecao_Vaginal = dplyr::case_when(
        PROC_VAGIN == 1 ~ "1 - Sim",
        PROC_VAGIN == 2 ~ "0 - Nao",
        PROC_VAGIN == 8 ~ "0 - Nao",
        PROC_VAGIN == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Contracepcao_Emergencia = dplyr::case_when(
        PROC_CONTR == 1 ~ "1 - Sim",
        PROC_CONTR == 2 ~ "0 - Nao",
        PROC_CONTR == 8 ~ "0 - Nao",
        PROC_CONTR == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Aborto_Previsto_Lei = dplyr::case_when(
        PROC_ABORT == 1 ~ "1 - Sim",
        PROC_ABORT == 2 ~ "0 - Nao",
        PROC_ABORT == 8 ~ "0 - Nao",
        PROC_ABORT == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      )
    )

    # -------------------------
    # 4. Remover colunas brutas da violência
    # -------------------------
    remove_vars <- c(
      "DT_OCOR", "DT_NOTIFIC",
      "OUT_VEZES", "LES_AUTOP", "VIOL_MOTIV",
      "VIOL_FISIC", "VIOL_PSICO", "VIOL_TORT", "VIOL_SEXU",
      "VIOL_FINAN", "VIOL_NEGLI", "VIOL_INFAN", "VIOL_LEGAL",
      "VIOL_OUTR", "VIOL_ESPEC",
      "SEX_ASSEDI", "SEX_ESTUPR", "SEX_PORNO", "SEX_EXPLO",
      "SEX_OUTRO", "SEX_ESPEC",
      "PROC_DST", "PROC_HIV", "PROC_HEPB", "PROC_SANG",
      "PROC_SEMEN", "PROC_VAGINA", "PROC_CONTR", "PROC_ABORT"
    )

    df <- dplyr::select(df, -dplyr::any_of(remove_vars))

    # -------------------------
    # 5. Atualizar pipe
    # -------------------------
    pipe$data$proc$data <- df
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "violence")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "violence")

    return(pipe)
  }
}
