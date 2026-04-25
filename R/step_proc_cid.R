# =========================
# STEP: CID + CARGA DE DOENÇA
# =========================

<<<<<<< HEAD
step_proc_cid <- function(expectativa_vida = 76.6,
                          usar_pesos_daly = TRUE) {

  function(pipe) {
=======
step_proc_cid <- function(expectativa_vida = 76.6) {

  function(data) {

    df <- data$data
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0

    # -------------------------
    # 1. Validação
    # -------------------------
<<<<<<< HEAD
    if (is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_cid()`.")
    }

    df <- pipe$data$proc$data

    required_vars <- c(
      "CAUSABAS",
=======
    required_vars <- c(
      "Causa_Basica",
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
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
<<<<<<< HEAD
          "Variáveis ausentes para `step_proc_cid()`: ",
=======
          "Variáveis ausentes no step_proc_cid(): ",
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
          paste(missing_vars, collapse = ", ")
        )
      )
    }

    # -------------------------
<<<<<<< HEAD
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

    # Normalizar códigos CID do arquivo interno
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
          toupper(trimws(as.character(CAUSABAS))),
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
=======
    # 2. Dicionário CID (interno)
    # -------------------------
    cid_dict <- dataLGBT::cid_dict %>%
      dplyr::mutate(
        ICD10_Cat = stringr::str_remove_all(ICD10_Cat, fixed("."))
      )

    # -------------------------
    # 3. Preparar CID do banco
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        CID10_raw = stringr::str_remove_all(Causa_Basica, fixed(".")),

        # Garante categoria de 3 caracteres
        CID10_Cat = substr(CID10_raw, 1, 3)
      ) %>%
      dplyr::left_join(cid_dict, by = c("CID10_Cat" = "ICD10_Cat"))

    # -------------------------
    # 4. Identificar óbito
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Obito = dplyr::case_when(
<<<<<<< HEAD
          is.na(Data_Obito) ~ "0 - Nao",
          TRUE ~ "1 - Sim"
        ),

        Violencia_Relacionada = dplyr::case_when(
          Obito == "1 - Sim" &
            Group_Code %in% c("245 (X85-Y09)", "246 (Y10-Y34)", "247 (Y35-Y36)") ~ "1 - Homicidio",

          Obito == "1 - Sim" &
            Group_Code == "244 (X60-X84)" ~ "2 - Suicidio",

          Obito == "0 - Nao" ~ "0 - Nao",
=======
          is.na(Data_Obito) ~ "0 - Não",
          TRUE ~ "1 - Sim"
        )
      )

    # -------------------------
    # 5. Classificação da causa
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Violencia_Relacionada = dplyr::case_when(

          Obito == "1 - Sim" &
            Group_Code %in% c("245 (X85-Y09)", "246 (Y10-Y34)", "247 (Y35-Y36)") ~ "1 - Homicídio",

          Obito == "1 - Sim" &
            Group_Code == "244 (X60-X84)" ~ "2 - Suicídio",

          Obito == "0 - Não" ~ "0 - Não",
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0

          TRUE ~ "3 - Outras causas"
        )
      )

    # -------------------------
<<<<<<< HEAD
    # 6. Tempo de sobrevivência
=======
    # 6. Tempo de sobrevivência (anos)
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Tempo_Sobrevivencia = dplyr::case_when(
<<<<<<< HEAD
          !is.na(Data_Violencia) & !is.na(Data_Obito) ~
            as.numeric(difftime(Data_Obito, Data_Violencia, units = "days")) / 365.25,

          is.na(Data_Obito) & !is.na(Idade_Violencia) ~
            expectativa_vida - as.numeric(Idade_Violencia),

          TRUE ~ NA_real_
        ),

=======
          Obito == "1 - Sim" ~ as.numeric(difftime(Data_Obito, Data_Violencia, units = "days")) / 365.25,
          TRUE ~ expectativa_vida - Idade_Violencia
        ),
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
        Tempo_Sobrevivencia = ifelse(Tempo_Sobrevivencia < 0, 0, Tempo_Sobrevivencia)
      )

    # -------------------------
<<<<<<< HEAD
    # 7. APVP
=======
    # 7. APVP (YLL)
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        APVP = dplyr::case_when(
<<<<<<< HEAD
          Obito == "1 - Sim" ~ expectativa_vida - as.numeric(Idade_Obito),
          TRUE ~ 0
        ),

=======
          Obito == "1 - Sim" ~ expectativa_vida - Idade_Obito,
          TRUE ~ 0
        ),
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
        APVP = ifelse(APVP < 0, 0, APVP)
      )

    # -------------------------
    # 8. AVCI (YLD)
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        .peso_incapacidade = dplyr::case_when(
<<<<<<< HEAD
          isTRUE(usar_pesos_daly) & Violencia_Fisica_Ampla == "1 - Sim" ~ 0.165,
          isTRUE(usar_pesos_daly) ~ 0.133,
          TRUE ~ 1
        ),

        AVCI = dplyr::case_when(
          is.na(Tempo_Sobrevivencia) ~ NA_real_,
          TRUE ~ Tempo_Sobrevivencia * .peso_incapacidade
        )
=======

          # Violência física ampla (prioritária)
          Violencia_Fisica_Ampla == "1 - Sim" ~ 0.165,

          # Demais casos → proxy psicológico
          TRUE ~ 0.133
        ),

        AVCI = Tempo_Sobrevivencia * .peso_incapacidade
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
      )

    # -------------------------
    # 9. Limpeza
    # -------------------------
    df$.peso_incapacidade <- NULL

    # -------------------------
<<<<<<< HEAD
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
=======
    # 10. Atualizar pipe
    # -------------------------
    data$data <- df

    data$log$proc_cid <- list(
      expectativa_vida = expectativa_vida,
      n = nrow(df),
      obitos = sum(df$Obito == "1 - Sim", na.rm = TRUE),
      media_apvp = mean(df$APVP, na.rm = TRUE),
      media_avci = mean(df$AVCI, na.rm = TRUE)
    )

    return(data)
>>>>>>> 25b9e8def7504751c87d0b0894cd8abc64040af0
  }
}
