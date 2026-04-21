# =========================
# STEP: DEDUP (PROBABILISTIC - fastLink)
# =========================

step_dedup <- function(dedup_sim = FALSE,
                       threshold = 0.85) {

  function(data) {

    sinan <- data$sinan_raw
    sim   <- data$sim_raw

    # -------------------------
    # 1. Validar variáveis padronizadas
    # -------------------------
    required_std <- c(".name_std", ".mother_name_std", ".birth_date_std")

    if (!all(required_std %in% names(sinan))) {
      stop("SINAN não foi padronizado. Execute `step_standardize()` antes.")
    }

    if (!all(required_std %in% names(sim))) {
      stop("SIM não foi padronizado. Execute `step_standardize()` antes.")
    }

    # -------------------------
    # 2. Helper: deduplicação probabilística
    # -------------------------
    dedup_fastlink <- function(df, dataset_name) {

      # Criar ID interno
      df$.id_internal <- seq_len(nrow(df))

      # fastLink requer dois datasets → usamos o mesmo (dedup)
      fl <- fastLink::fastLink(
        dfA = df,
        dfB = df,
        varnames = c(".name_std", ".mother_name_std", ".birth_date_std"),
        stringdist.match = c(".name_std", ".mother_name_std"),
        partial.match = c(".name_std", ".mother_name_std"),
        numeric.match = NULL,
        threshold.match = threshold
      )

      # Obter pares
      pairs <- fastLink::getMatches(
        fl,
        dfA = df,
        dfB = df,
        threshold = threshold
      )

      if (nrow(pairs) == 0) {
        df$.id_internal <- NULL
        return(df)
      }

      # Remover auto-match (i == j)
      pairs <- pairs[pairs$id1 != pairs$id2, ]

      if (nrow(pairs) == 0) {
        df$.id_internal <- NULL
        return(df)
      }

      # -------------------------
      # 3. Criar clusters simples
      # -------------------------

      # Estratégia simples: manter menor id em cada grupo
      remove_ids <- unique(pairs$id2)

      df <- df[!df$.id_internal %in% remove_ids, ]

      df$.id_internal <- NULL

      return(df)
    }

    # -------------------------
    # 4. Deduplicar SINAN (obrigatório)
    # -------------------------
    sinan_dedup <- dedup_fastlink(sinan, "sinan")

    # -------------------------
    # 5. Deduplicar SIM (opcional)
    # -------------------------
    if (dedup_sim) {
      sim_dedup <- dedup_fastlink(sim, "sim")
    } else {
      sim_dedup <- sim
    }

    # -------------------------
    # 6. Atualizar estrutura
    # -------------------------
    data$sinan_raw <- sinan_dedup
    data$sim_raw   <- sim_dedup

    return(data)
  }
}
