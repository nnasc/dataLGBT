# =========================
# STEP: VIOLÊNCIA
# =========================

step_proc_violence <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_violence()`.")
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
        !is.na(DT_OCOR.x) ~ DT_OCOR.x,
        is.na(DT_OCOR.x) & !is.na(DT_NOTIFIC.x) ~ DT_NOTIFIC.x,
        TRUE ~ NA
      ),

      # Violência em repetição
      Violencia_Repetida = dplyr::case_when(
        OUT_VEZES.x == 1 ~ "1 - Sim",
        OUT_VEZES.x == 2 ~ "0 - Nao",
        OUT_VEZES.x == 9 ~ NA_character_,
        TRUE ~ NA_character_
      ),

      # Lesão autoprovocada
      Lesao_Autoprovocada = dplyr::case_when(
        LES_AUTOP.x == 1 ~ "1 - Sim",
        LES_AUTOP.x == 2 ~ "0 - Nao",
        LES_AUTOP.x == 8 ~ "0 - Nao",
        LES_AUTOP.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      # Violência motivada por
      Violencia_Motivada = dplyr::case_when(
        VIOL_MOTIV.x == 1 ~ "Sexismo",
        VIOL_MOTIV.x == 2 ~ "Homofobia/Lesbofobia/Bifobia/Transfobia",
        VIOL_MOTIV.x == 3 ~ "Racismo",
        VIOL_MOTIV.x == 4 ~ "Intolerancia religiosa",
        VIOL_MOTIV.x == 5 ~ "Xenofobia",
        VIOL_MOTIV.x == 6 ~ "Conflito geracional",
        VIOL_MOTIV.x == 7 ~ "Situacao de rua",
        VIOL_MOTIV.x == 8 ~ "Deficiencia",
        VIOL_MOTIV.x == 9 ~ NA_character_,
        VIOL_MOTIV.x == 88 ~ NA_character_,
        VIOL_MOTIV.x == 99 ~ NA_character_,
        TRUE ~ NA_character_
      ),

      # Tipo de violência
      Violencia_Fisica = dplyr::case_when(
        VIOL_FISIC.x == 1 ~ "1 - Sim",
        VIOL_FISIC.x == 2 ~ "0 - Nao",
        VIOL_FISIC.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Psico_Moral = dplyr::case_when(
        VIOL_PSICO.x == 1 ~ "1 - Sim",
        VIOL_PSICO.x == 2 ~ "0 - Nao",
        VIOL_PSICO.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Tortura = dplyr::case_when(
        VIOL_TORT.x == 1 ~ "1 - Sim",
        VIOL_TORT.x == 2 ~ "0 - Nao",
        VIOL_TORT.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Sexual = dplyr::case_when(
        VIOL_SEXU.x == 1 ~ "1 - Sim",
        VIOL_SEXU.x == 2 ~ "0 - Nao",
        VIOL_SEXU.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Violencia_Financeira = dplyr::case_when(
        VIOL_FINAN.x == 1 ~ "1 - Sim",
        VIOL_FINAN.x == 2 ~ "0 - Nao",
        VIOL_FINAN.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Negligencia_Abandono = dplyr::case_when(
        VIOL_NEGLI.x == 1 ~ "1 - Sim",
        VIOL_NEGLI.x == 2 ~ "0 - Nao",
        VIOL_NEGLI.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Trabalho_Infantil = dplyr::case_when(
        VIOL_INFAN.x == 1 ~ "1 - Sim",
        VIOL_INFAN.x == 2 ~ "0 - Nao",
        VIOL_INFAN.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Intervencao_Legal = dplyr::case_when(
        VIOL_LEGAL.x == 1 ~ "1 - Sim",
        VIOL_LEGAL.x == 2 ~ "0 - Nao",
        VIOL_LEGAL.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Tipo_Violencia = dplyr::case_when(
        VIOL_OUTR.x == 1 ~ "1 - Sim",
        VIOL_OUTR.x == 2 ~ "0 - Nao",
        VIOL_OUTR.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Tipo_Violencia_Especificar = dplyr::if_else(
        VIOL_OUTR.x == 1,
        as.character(VIOL_ESPEC.x),
        NA_character_
      ),

      Violencia_Fisica_Ampla = dplyr::case_when(

        # Qualquer violência com componente físico
        VIOL_FIS.x  == 1 |
          VIOL_SEXU.x == 1 |
          VIOL_TORT.x == 1 |
          TRA_INF.x   == 1 |
          TRAB_INF.x  == 1 |
          VIOL_NEGL.x == 1 |
          VIOL_OUTR.x == 1 ~ "1 - Sim",

        # Caso contrário → Não
        TRUE ~ "0 - Não"
      ),

      # Violência sexual - subtipos
      Assedio_Sexual = dplyr::case_when(
        SEX_ASSEDI.x == 1 ~ "1 - Sim",
        SEX_ASSEDI.x == 2 ~ "0 - Nao",
        SEX_ASSEDI.x == 8 ~ "0 - Nao",
        SEX_ASSEDI.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Estupro = dplyr::case_when(
        SEX_ESTUPR.x == 1 ~ "1 - Sim",
        SEX_ESTUPR.x == 2 ~ "0 - Nao",
        SEX_ESTUPR.x == 8 ~ "0 - Nao",
        SEX_ESTUPR.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Pornografia_Infantil = dplyr::case_when(
        SEX_PORNO.x == 1 ~ "1 - Sim",
        SEX_PORNO.x == 2 ~ "0 - Nao",
        SEX_PORNO.x == 8 ~ "0 - Nao",
        SEX_PORNO.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Exploracao_Sexual = dplyr::case_when(
        SEX_EXPLO.x == 1 ~ "1 - Sim",
        SEX_EXPLO.x == 2 ~ "0 - Nao",
        SEX_EXPLO.x == 8 ~ "0 - Nao",
        SEX_EXPLO.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Sexual = dplyr::case_when(
        SEX_OUTRO.x == 1 ~ "1 - Sim",
        SEX_OUTRO.x == 2 ~ "0 - Nao",
        SEX_OUTRO.x == 8 ~ "0 - Nao",
        SEX_OUTRO.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Outro_Sexual_Especificar = dplyr::if_else(
        SEX_OUTRO.x == 1,
        as.character(SEX_ESPEC.x),
        NA_character_
      ),

      # Procedimentos pós-violência sexual
      Profilaxia_DST = dplyr::case_when(
        PROC_DST.x == 1 ~ "1 - Sim",
        PROC_DST.x == 2 ~ "0 - Nao",
        PROC_DST.x == 8 ~ "0 - Nao",
        PROC_DST.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Profilaxia_HIV = dplyr::case_when(
        PROC_HIV.x == 1 ~ "1 - Sim",
        PROC_HIV.x == 2 ~ "0 - Nao",
        PROC_HIV.x == 8 ~ "0 - Nao",
        PROC_HIV.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Profilaxia_Hepatite_B = dplyr::case_when(
        PROC_HEPB.x == 1 ~ "1 - Sim",
        PROC_HEPB.x == 2 ~ "0 - Nao",
        PROC_HEPB.x == 8 ~ "0 - Nao",
        PROC_HEPB.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Sangue = dplyr::case_when(
        PROC_SANG.x == 1 ~ "1 - Sim",
        PROC_SANG.x == 2 ~ "0 - Nao",
        PROC_SANG.x == 8 ~ "0 - Nao",
        PROC_SANG.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Semen = dplyr::case_when(
        PROC_SEMEN.x == 1 ~ "1 - Sim",
        PROC_SEMEN.x == 2 ~ "0 - Nao",
        PROC_SEMEN.x == 8 ~ "0 - Nao",
        PROC_SEMEN.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Coleta_Secrecao_Vaginal = dplyr::case_when(
        PROC_VAGINA.x == 1 ~ "1 - Sim",
        PROC_VAGINA.x == 2 ~ "0 - Nao",
        PROC_VAGINA.x == 8 ~ "0 - Nao",
        PROC_VAGINA.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Contracepcao_Emergencia = dplyr::case_when(
        PROC_CONTR.x == 1 ~ "1 - Sim",
        PROC_CONTR.x == 2 ~ "0 - Nao",
        PROC_CONTR.x == 8 ~ "0 - Nao",
        PROC_CONTR.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      ),

      Aborto_Previsto_Lei = dplyr::case_when(
        PROC_ABORT.x == 1 ~ "1 - Sim",
        PROC_ABORT.x == 2 ~ "0 - Nao",
        PROC_ABORT.x == 8 ~ "0 - Nao",
        PROC_ABORT.x == 9 ~ "0 - Nao",
        TRUE ~ NA_character_
      )
    )

    # -------------------------
    # 4. Remover colunas brutas da violência
    # -------------------------
    remove_vars <- c(
      "DT_OCOR.x", "DT_NOTIFIC.x",
      "OUT_VEZES.x", "LES_AUTOP.x", "VIOL_MOTIV.x",
      "VIOL_FISIC.x", "VIOL_PSICO.x", "VIOL_TORT.x", "VIOL_SEXU.x",
      "VIOL_FINAN.x", "VIOL_NEGLI.x", "VIOL_INFAN.x", "VIOL_LEGAL.x",
      "VIOL_OUTR.x", "VIOL_ESPEC.x",
      "SEX_ASSEDI.x", "SEX_ESTUPR.x", "SEX_PORNO.x", "SEX_EXPLO.x",
      "SEX_OUTRO.x", "SEX_ESPEC.x",
      "PROC_DST.x", "PROC_HIV.x", "PROC_HEPB.x", "PROC_SANG.x",
      "PROC_SEMEN.x", "PROC_VAGINA.x", "PROC_CONTR.x", "PROC_ABORT.x"
    )

    df <- dplyr::select(df, -dplyr::any_of(remove_vars))

    # -------------------------
    # 5. Atualizar pipe
    # -------------------------
    pipe$data$proc$data <- df

    pipe$data$proc$steps <- c(
      pipe$data$proc$steps,
      "violencia"
    )

    pipe$proc_meta$steps <- c(
      pipe$proc_meta$steps,
      "violencia"
    )

    return(pipe)
  }
}
