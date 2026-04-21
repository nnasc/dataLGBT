# =========================
# STEP: FILTER SOGI
# =========================

step_filter_sogi <- function(filter_sogi = c("all","lgbt","gay","lesbian",
                                             "bissexual","travesti",
                                             "mul.trans","hom.trans")) {
  
  filter_sogi <- match.arg(filter_sogi)
  
  function(data) {
    
    sinan <- data$sinan_raw
    
    # -------------------------
    # 1. Validar colunas necessárias
    # -------------------------
    required_cols <- c("ORIENT_SEX", "IDENT_GEN", "CS_SEXO")
    
    if (!all(required_cols %in% names(sinan))) {
      stop("SINAN não possui variáveis necessárias para filtro SOGI.")
    }
    
    # -------------------------
    # 2. Criar variável SOGI
    # -------------------------
    sinan <- sinan %>%
      dplyr::mutate(
        sogi_group = dplyr::case_when(
          
          # Gay
          ORIENT_SEX == 2 & 
            (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) & 
            CS_SEXO == "M" ~ "gay",
          
          # Lésbica
          ORIENT_SEX == 2 & 
            (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) & 
            CS_SEXO == "F" ~ "lesbian",
          
          # Bissexual
          ORIENT_SEX == 3 & 
            (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) ~ "bissexual",
          
          # Travesti
          IDENT_GEN == 1 ~ "travesti",
          
          # Mulher trans
          IDENT_GEN == 2 ~ "mul.trans",
          
          # Homem trans
          IDENT_GEN == 3 ~ "hom.trans",
          
          TRUE ~ "nao_lgbt"
        )
      )
    
    # -------------------------
    # 3. Aplicar filtro
    # -------------------------
    if (filter_sogi == "all") {
      
      # não filtra
      
    } else if (filter_sogi == "lgbt") {
      
      sinan <- sinan %>%
        dplyr::filter(sogi_group != "nao_lgbt")
      
    } else {
      
      sinan <- sinan %>%
        dplyr::filter(sogi_group == filter_sogi)
    }
    
    data$sinan_raw <- sinan
    
    return(data)
  }
}