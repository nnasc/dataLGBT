# =========================
# STEP: CLEAN
# =========================

step_clean <- function(remove_missing_name = TRUE,
                       remove_missing_mother = FALSE,
                       remove_missing_birth = FALSE,
                       min_name_length = 3) {
  
  function(data) {
    
    # -------------------------
    # 1. Extrair dados
    # -------------------------
    sinan <- data$sinan_raw
    sim   <- data$sim_raw
    
    # -------------------------
    # 2. Validar variáveis padronizadas
    # -------------------------
    required_std <- c(".name_std", ".mother_name_std", ".birth_date_std")
    
    if (!all(required_std %in% names(sinan))) {
      stop("SINAN não foi padronizado. Execute `step_standardize()` antes.")
    }
    
    if (!all(required_std %in% names(sim))) {
      stop("SIM não foi padronizado. Execute `step_standardize()` antes.")
    }
    
    # -------------------------
    # 3. Função auxiliar de limpeza
    # -------------------------
    clean_df <- function(df) {
      
      # Remover nomes muito curtos
      if (!is.null(min_name_length)) {
        df <- df %>%
          dplyr::filter(
            is.na(.name_std) | nchar(.name_std) >= min_name_length
          )
      }
      
      # Remover nome ausente
      if (remove_missing_name) {
        df <- df %>%
          dplyr::filter(!is.na(.name_std))
      }
      
      # Remover nome da mãe ausente
      if (remove_missing_mother) {
        df <- df %>%
          dplyr::filter(!is.na(.mother_name_std))
      }
      
      # Remover data de nascimento ausente
      if (remove_missing_birth) {
        df <- df %>%
          dplyr::filter(!is.na(.birth_date_std))
      }
      
      # Remover datas absurdas (opcional básico)
      df <- df %>%
        dplyr::filter(
          is.na(.birth_date_std) |
            (.birth_date_std >= as.Date("1900-01-01") &
               .birth_date_std <= Sys.Date())
        )
      
      return(df)
    }
    
    # -------------------------
    # 4. Aplicar limpeza
    # -------------------------
    sinan <- clean_df(sinan)
    sim   <- clean_df(sim)
    
    # -------------------------
    # 5. Atualizar estrutura
    # -------------------------
    data$sinan_raw <- sinan
    data$sim_raw   <- sim
    
    return(data)
  }
}