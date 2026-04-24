# =========================
# STEP: CID + CARGA DE DOENÇA
# =========================

step_proc_cid <- function(expectativa_vida = 76.6) {

  function(data) {

    df <- data$data

    # -------------------------
    # 1. Validação
    # -------------------------
    required_vars <- c(
      "Causa_Basica",
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
          "Variáveis ausentes no step_proc_cid(): ",
          paste(missing_vars, collapse = ", ")
        )
      )
    }

    # -------------------------
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
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Obito = dplyr::case_when(
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

          TRUE ~ "3 - Outras causas"
        )
      )

    # -------------------------
    # 6. Tempo de sobrevivência (anos)
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        Tempo_Sobrevivencia = dplyr::case_when(
          Obito == "1 - Sim" ~ as.numeric(difftime(Data_Obito, Data_Violencia, units = "days")) / 365.25,
          TRUE ~ expectativa_vida - Idade_Violencia
        ),
        Tempo_Sobrevivencia = ifelse(Tempo_Sobrevivencia < 0, 0, Tempo_Sobrevivencia)
      )

    # -------------------------
    # 7. APVP (YLL)
    # -------------------------
    df <- df %>%
      dplyr::mutate(
        APVP = dplyr::case_when(
          Obito == "1 - Sim" ~ expectativa_vida - Idade_Obito,
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

          # Violência física ampla (prioritária)
          Violencia_Fisica_Ampla == "1 - Sim" ~ 0.165,

          # Demais casos → proxy psicológico
          TRUE ~ 0.133
        ),

        AVCI = Tempo_Sobrevivencia * .peso_incapacidade
      )

    # -------------------------
    # 9. Limpeza
    # -------------------------
    df$.peso_incapacidade <- NULL

    # -------------------------
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
  }
}
