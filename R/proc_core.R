# =========================
# PROC CORE
# =========================

proc_core <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação de entrada
    # -------------------------
    if (!inherits(pipe, "data_link_pipe")) {
      stop("`data_proc()` requer um objeto da classe 'data_link_pipe'. Execute `data_link()` primeiro.")
    }

    if (is.null(pipe$data$linkage)) {
      stop("Nenhum objeto de linkage encontrado no pipe.")
    }

    if (is.null(pipe$data$linkage$matches) || nrow(pipe$data$linkage$matches) == 0) {
      stop("Nenhum match disponível. Verifique o step_linkage().")
    }

    # -------------------------
    # 2. Selecionar base principal
    # -------------------------
    df <- pipe$data$linkage$matches

    # -------------------------
    # 3. Sanitização mínima
    # -------------------------
    # Evita problemas com factors (muito comum em SIM/SINAN)
    df <- dplyr::mutate(
      df,
      dplyr::across(where(is.factor), as.character)
    )

    # -------------------------
    # 4. Estrutura inicial do proc
    # -------------------------
    pipe$data$proc <- list(
      data = df,
      steps = character(0)
    )

    # -------------------------
    # 5. Metadata inicial
    # -------------------------
    pipe$proc_meta <- list(
      n_input_sinan = nrow(pipe$data$sinan_raw),
      n_input_sim   = nrow(pipe$data$sim_raw),
      n_matches     = nrow(df),
      steps         = character(0),
      timestamp     = Sys.time()
    )

    return(pipe)
  }
}
