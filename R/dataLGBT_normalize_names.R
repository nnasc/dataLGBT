# =========================
# NORMALIZADOR DE COLUNAS
# =========================

.dataLGBT_normalize_names <- function(df) {

  # nomes base (sem .x/.y)
  base_names <- unique(gsub("\\.(x|y)$", "", names(df)))

  for (var in base_names) {

    candidates <- c(
      paste0(var, ".x"),
      var,
      paste0(var, ".y")
    )

    candidates <- candidates[candidates %in% names(df)]

    if (length(candidates) == 0) next

    df[[var]] <- dplyr::coalesce(!!!rlang::syms(candidates))
  }

  # remover colunas duplicadas
  df <- df[, !grepl("\\.(x|y)$", names(df)), drop = FALSE]

  return(df)
}
