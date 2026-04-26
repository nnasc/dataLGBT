# =========================
# STEP: CID + CARGA DE DOENÇA
# =========================

step_proc_cid <- function(expectativa_vida = 76.6,
                          usar_pesos_daly = TRUE) {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_cid()`.")
    }

    df <- pipe$data$proc$data

    # Aceita os dois nomes possíveis para a causa básica
    causa_col <- dplyr::case_when(
      "Causa_Basica" %in% names(df) ~ "Causa_Basica",
      "CAUSABAS" %in% names(df) ~ "CAUSABAS",
      TRUE ~ NA_character_
    )

    if (is.na(causa_col)) {
      stop("Variável de causa básica ausente: esperava `CAUSABAS` ou `Causa_Basica`.")
    }

    required_vars <- c(
      causa_col,
      "Data_Obito",
      "Data_Violencia",
      "Idade_Violencia",
      "Idade_Obito",
      "Violencia_Fisica_Ampla"
    )

    missing_vars <- setdiff(required_vars, names(df))

    if (length(missing_vars) > 0) {
      stop(
        paste0(
          "Variáveis ausentes para `step_proc_cid()`: ",
          paste(missing_vars, collapse = ", ")
        )
      )
    }

    # -------------------------
    # 2. Garantir tipos
    # -------------------------
    df <- dplyr::mutate(
      df,
      dplyr::across(where(is.factor), as.character)
    )

    # -------------------------
    # 3. Carregar CID interno do pacote
    # -------------------------
    path_cid <- system.file(
      "extdata",
      "cid10_grupos.csv",
      package = "dataLGBT"
    )

    if (path_cid == "") {
      stop("Arquivo de CID não encontrado em `inst/extdata/cid10_grupos.csv`.")
    }

    cid <- readr::read_csv(path_cid, show_col_types = FALSE)

    required_cid_vars <- c("ICD10_Code", "Group_Code")
    missing_cid_vars <- setdiff(required_cid_vars, names(cid))

    if (length(missing_cid_vars) > 0) {
      stop(
        paste0(
          "O arquivo CID precisa conter as colunas: ",
          paste(required_cid_vars, collapse = ", ")
        )
      )
    }

    cid <- cid %>%
      dplyr::mutate(
        ICD10_Code = stringr::str_remove_all(
          toupper(trimws(as.character(ICD10_Code))),
          stringr::fixed(".")
        )
      )

    # -------------------------
    # 4. Preparar CID na base principal
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Causa_Basica = stringr::str_remove_all(
          toupper(trimws(as.character(.data[[causa_col]]))),
          stringr::fixed(".")
        ),
        CID10_Cat = substr(Causa_Basica, 1, 3)
      ) %>%
      dplyr::left_join(
        cid,
        by = c("CID10_Cat" = "ICD10_Code")
      )

    if (!"Group_Code" %in% names(df)) {
      stop("A junção com a tabela CID não funcionou corretamente.")
    }

    # -------------------------
    # 5. Identificar óbito e classificar causa
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Obito = dplyr::case_when(
          is.na(Data_Obito) ~ "0 - Nao",
          TRUE ~ "1 - Sim"
        ),
        Violencia_Relacionada = dplyr::case_when(
          Obito == "1 - Sim" &
            Group_Code %in% c("245 (X85-Y09)", "246 (Y10-Y34)", "247 (Y35-Y36)") ~ "1 - Homicidio",
          Obito == "1 - Sim" &
            Group_Code == "244 (X60-X84)" ~ "2 - Suicidio",
          Obito == "0 - Nao" ~ "0 - Nao",
          TRUE ~ "3 - Outras causas"
        )
      )

    # -------------------------
    # 6. Tempo de sobrevivência
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Tempo_Sobrevivencia = dplyr::case_when(
          !is.na(Data_Obito) & !is.na(Data_Violencia) ~
            as.numeric(difftime(Data_Obito, Data_Violencia, units = "days")) / 365.25,
          is.na(Data_Obito) & !is.na(Idade_Violencia) ~
            expectativa_vida - as.numeric(Idade_Violencia),
          TRUE ~ NA_real_
        ),
        Tempo_Sobrevivencia = ifelse(Tempo_Sobrevivencia < 0, 0, Tempo_Sobrevivencia)
      )

    # -------------------------
    # 7. APVP
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        APVP = dplyr::case_when(
          Obito == "1 - Sim" ~ expectativa_vida - as.numeric(Idade_Obito),
          TRUE ~ 0
        ),
        APVP = ifelse(APVP < 0, 0, APVP)
      )

    # -------------------------
    # 8. AVCI (YLD)
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        .peso_incapacidade = dplyr::case_when(
          isTRUE(usar_pesos_daly) & Violencia_Fisica_Ampla == "1 - Sim" ~ 0.165,
          isTRUE(usar_pesos_daly) ~ 0.133,
          TRUE ~ 1
        ),
        AVCI = dplyr::case_when(
          is.na(Tempo_Sobrevivencia) ~ NA_real_,
          TRUE ~ Tempo_Sobrevivencia * .peso_incapacidade
        )
      )

    # -------------------------
    # 9. Limpeza
    # -------------------------
    df$.peso_incapacidade <- NULL

    # -------------------------
    # 10. Atualizar pipeline
    # -------------------------
    pipe$data$proc$data <- df

    pipe$data$proc$steps <- c(
      pipe$data$proc$steps,
      "cid"
    )

    pipe$proc_meta$steps <- c(
      pipe$proc_meta$steps,
      "cid"
    )

    return(pipe)
  }
}
