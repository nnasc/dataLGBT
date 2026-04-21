# =========================
# STEP: LINKAGE (fastLink)
# =========================

step_linkage <- function(threshold = 0.35,
                         return_pairs = TRUE) {
  
  function(data) {
    
    sinan <- data$sinan_raw
    sim   <- data$sim_raw
    
    # -------------------------
    # 1. Validação
    # -------------------------
    required_std <- c(".name_std", ".mother_name_std", ".birth_date_std")
    
    if (!all(required_std %in% names(sinan))) {
      stop("SINAN não foi padronizado.")
    }
    
    if (!all(required_std %in% names(sim))) {
      stop("SIM não foi padronizado.")
    }
    
    # -------------------------
    # 2. IDs internos
    # -------------------------
    sinan$.id_sinan <- seq_len(nrow(sinan))
    sim$.id_sim     <- seq_len(nrow(sim))
    
    # -------------------------
    # 3. Blocking
    # -------------------------
    sinan <- sinan %>%
      dplyr::mutate(
        .block = paste0(
          substr(.name_std, 1, 1),
          format(.birth_date_std, "%Y")
        )
      )
    
    sim <- sim %>%
      dplyr::mutate(
        .block = paste0(
          substr(.name_std, 1, 1),
          format(.birth_date_std, "%Y")
        )
      )
    
    common_blocks <- intersect(unique(sinan$.block), unique(sim$.block))
    
    sinan_block <- sinan %>% dplyr::filter(.block %in% common_blocks)
    sim_block   <- sim   %>% dplyr::filter(.block %in% common_blocks)
    
    if (nrow(sinan_block) == 0 || nrow(sim_block) == 0) {
      
      warning("Nenhum bloco comum encontrado. Linkage vazio.")
      
      data$linkage <- list(
        model   = NULL,
        pairs   = NULL,
        matches = NULL
      )
      
      return(data)
    }
    
    # -------------------------
    # 4. fastLink
    # -------------------------
    fl <- fastLink::fastLink(
      dfA = sinan_block,
      dfB = sim_block,
      varnames = c(".name_std", ".mother_name_std", ".birth_date_std"),
      stringdist.match = c(".name_std", ".mother_name_std"),
      partial.match = c(".name_std", ".mother_name_std"),
      threshold.match = threshold
    )
    
    # -------------------------
    # 5. Pares
    # -------------------------
    pairs <- fastLink::getMatches(
      fl,
      dfA = sinan_block,
      dfB = sim_block,
      threshold = threshold
    )
    
    if (is.null(pairs) || nrow(pairs) == 0) {
      
      data$linkage <- list(
        model   = fl,
        pairs   = NULL,
        matches = NULL
      )
      
      return(data)
    }
    
    # Organizar pares
    pairs_out <- if ("posterior" %in% names(pairs)) {
      pairs %>% dplyr::select(id1, id2, posterior)
    } else {
      pairs
    }
    
    # -------------------------
    # 6. Matching 1:1 (greedy)
    # -------------------------
    pairs_ord <- if ("posterior" %in% names(pairs)) {
      pairs %>% dplyr::arrange(dplyr::desc(posterior))
    } else {
      pairs
    }
    
    matched_sinan <- c()
    matched_sim   <- c()
    final_matches <- list()
    
    for (i in seq_len(nrow(pairs_ord))) {
      
      s <- pairs_ord$id1[i]
      t <- pairs_ord$id2[i]
      
      if (!(s %in% matched_sinan) && !(t %in% matched_sim)) {
        
        matched_sinan <- c(matched_sinan, s)
        matched_sim   <- c(matched_sim, t)
        
        final_matches[[length(final_matches) + 1]] <- pairs_ord[i, ]
      }
    }
    
    matches_out <- dplyr::bind_rows(final_matches)
    
    # -------------------------
    # 7. Salvar no pipe
    # -------------------------
    data$linkage <- list(
      model   = fl,
      pairs   = if (return_pairs) pairs_out else NULL,
      matches = matches_out
    )
    
    # Limpeza
    sinan$.id_sinan <- NULL
    sim$.id_sim     <- NULL
    sinan$.block    <- NULL
    sim$.block      <- NULL
    
    data$sinan_raw <- sinan
    data$sim_raw   <- sim
    
    return(data)
  }
}