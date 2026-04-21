# =========================
# STANDARDIZATION HELPERS
# =========================

normalize_string <- function(x) {
  
  if (is.factor(x)) x <- as.character(x)
  
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- trimws(x)
  
  # Remover múltiplos espaços
  x <- gsub("\\s+", " ", x)
  
  # Tratar strings vazias
  x[x == ""] <- NA_character_
  
  return(x)
}


normalize_date <- function(x) {
  
  # Se já for Date, mantém
  if (inherits(x, "Date")) {
    return(x)
  }
  
  # Tentar conversão padrão
  out <- as.Date(x)
  
  return(out)
}


# =========================
# STEP: STANDARDIZE
# =========================

step_standardize <- function() {
  
  function(data) {
    
    # -------------------------
    # 1. Extrair dados
    # -------------------------
    sinan <- data$sinan_raw
    sim   <- data$sim_raw
    
    # -------------------------
    # 2. Validar colunas mínimas
    # -------------------------
    required_sinan <- c("NM_PACIENT", "NM_MAE_PAC", "DT_NASC")
    required_sim   <- c("NOME", "NOMEMAE", "DTNASC")
    
    if (!all(required_sinan %in% names(sinan))) {
      stop("SINAN não possui colunas necessárias para padronização.")
    }
    
    if (!all(required_sim %in% names(sim))) {
      stop("SIM não possui colunas necessárias para padronização.")
    }
    
    # -------------------------
    # 3. Criar variáveis canônicas
    # -------------------------
    
    # SINAN
    sinan <- sinan %>%
      dplyr::mutate(
        .name_std         = normalize_string(NM_PACIENT),
        .mother_name_std  = normalize_string(NM_MAE_PAC),
        .birth_date_std   = normalize_date(DT_NASC)
      )
    
    # SIM
    sim <- sim %>%
      dplyr::mutate(
        .name_std         = normalize_string(NOME),
        .mother_name_std  = normalize_string(NOMEMAE),
        .birth_date_std   = normalize_date(DTNASC)
      )
    
    # -------------------------
    # 4. Atualizar estrutura
    # -------------------------
    data$sinan_raw <- sinan
    data$sim_raw   <- sim
    
    return(data)
  }
}