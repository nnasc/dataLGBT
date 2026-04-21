# =========================
# STEP: DEDUP (PROBABILISTIC - fastLink)
# =========================

step_dedup <- function(dedup_sim = FALSE,
                       threshold = 0.85) {

  function(data) {

    sinan <- data$sinan_raw
    sim   <- data$sim_raw

    # -------------------------
    # 1. Validate standardized variables
    # -------------------------
    required_std <- c(".name_std", ".mother_name_std", ".birth_date_std")

    if (!all(required_std %in% names(sinan))) {
      stop("SINAN was not standardized. Run `step_standardize()` first.")
    }

    if (!all(required_std %in% names(sim))) {
      stop("SIM was not standardized. Run `step_standardize()` first.")
    }

    # -------------------------
    # 2. Helper: probabilistic dedup
    # -------------------------
    dedup_fastlink <- function(df) {

      df$.row_id <- seq_len(nrow(df))

      fl <- fastLink::fastLink(
        dfA = df,
        dfB = df,
        varnames = c(".name_std", ".mother_name_std", ".birth_date_std"),
        stringdist.match = c(".name_std", ".mother_name_std"),
        partial.match = c(".name_std", ".mother_name_std"),
        numeric.match = NULL,
        threshold.match = threshold,
        verbose = TRUE
      )

      pairs <- fastLink::getMatches(
        fl,
        dfA = df,
        dfB = df,
        threshold = threshold
      )

      # Default audit info
      step_info <- list(
        threshold = threshold,
        n_input = nrow(df),
        n_output = nrow(df),
        n_clusters = 0L,
        n_rows_collapsed = 0L,
        posterior_max = NA_real_,
        posterior_min = NA_real_,
        posterior_mean = NA_real_
      )

      if (is.null(pairs) || nrow(pairs) == 0) {
        attr(df, "step_info") <- step_info
        df$.row_id <- NULL
        return(df)
      }

      if (!"dedupe.ids" %in% names(pairs)) {
        stop("fastLink did not return `dedupe.ids` in the dedup output.")
      }

      out <- as.data.frame(pairs)

      clustered <- out[!is.na(out$dedupe.ids), , drop = FALSE]

      if (nrow(clustered) == 0) {
        attr(df, "step_info") <- step_info
        df$.row_id <- NULL
        return(df)
      }

      # Posterior audit
      if ("posterior" %in% names(clustered)) {
        step_info$posterior_max  <- suppressWarnings(max(clustered$posterior, na.rm = TRUE))
        step_info$posterior_min  <- suppressWarnings(min(clustered$posterior, na.rm = TRUE))
        step_info$posterior_mean <- suppressWarnings(mean(clustered$posterior, na.rm = TRUE))

        clustered <- clustered[order(-clustered$posterior), , drop = FALSE]
      }

      # One representative per cluster
      clustered <- clustered[!duplicated(clustered$dedupe.ids), , drop = FALSE]

      # Count clusters and collapsed rows
      step_info$n_clusters <- length(unique(clustered$dedupe.ids))
      step_info$n_rows_collapsed <- sum(duplicated(out$dedupe.ids) & !is.na(out$dedupe.ids))

      # Keep selected representatives from the original data
      keep_ids <- unique(clustered$.row_id)
      keep_ids <- keep_ids[!is.na(keep_ids)]

      df_out <- df[df$.row_id %in% keep_ids, , drop = FALSE]
      df_out$.row_id <- NULL

      step_info$n_output <- nrow(df_out)

      attr(df_out, "step_info") <- step_info
      return(df_out)
    }

    # -------------------------
    # 3. Deduplicate SINAN (mandatory)
    # -------------------------
    sinan_dedup <- dedup_fastlink(sinan)

    # -------------------------
    # 4. Deduplicate SIM (optional)
    # -------------------------
    if (dedup_sim) {
      sim_dedup <- dedup_fastlink(sim)
    } else {
      sim_dedup <- sim
      attr(sim_dedup, "step_info") <- list(
        threshold = threshold,
        n_input = nrow(sim),
        n_output = nrow(sim),
        n_clusters = 0L,
        n_rows_collapsed = 0L,
        posterior_max = NA_real_,
        posterior_min = NA_real_,
        posterior_mean = NA_real_,
        skipped = TRUE
      )
    }

    # -------------------------
    # 5. Update pipe data
    # -------------------------
    data$sinan_raw <- sinan_dedup
    data$sim_raw   <- sim_dedup

    # Attach combined step info for pipe_add_step() to log
    attr(data, "step_info") <- list(
      dedup_sinan = attr(sinan_dedup, "step_info"),
      dedup_sim   = attr(sim_dedup, "step_info")
    )

    return(data)
  }
}
