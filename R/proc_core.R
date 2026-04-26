# =========================
# CORE: PROC
# =========================

proc_core <- function() {

  function(data_links) {

    # -------------------------------------------------------
    # 1. Validação
    # -------------------------------------------------------
    if (!inherits(data_links, "data_link_pipe")) {
      stop("`data_links` deve ser um objeto da classe 'data_link_pipe'.")
    }

    if (is.null(data_links$data$linkage$matches) ||
        !is.data.frame(data_links$data$linkage$matches)) {
      stop("`data_links$data$linkage$matches` deve ser um data.frame.")
    }

    # -------------------------------------------------------
    # 2. Extrair dados
    # -------------------------------------------------------
    df <- data_links$data$linkage$matches

    # -------------------------------------------------------
    # 3. NORMALIZAÇÃO DE NOMES (CRÍTICO)
    #    - remove sufixos .x / .y
    #    - padroniza nomes duplicados
    # -------------------------------------------------------
    normalize_names <- function(df) {

      nms <- names(df)

      # remove .x / .y
      nms <- gsub("\\.(x|y)$", "", nms)

      # garantir unicidade
      nms <- make.unique(nms)

      names(df) <- nms
      df
    }

    df <- normalize_names(df)

    # -------------------------------------------------------
    # 4. Criar estrutura do pipe
    # -------------------------------------------------------
    pipe <- list(
      data = list(
        linkage = data_links$data$linkage,  # mantém linkage
        proc = list(
          data = df,
          steps = character()
        )
      ),
      proc_meta = list(
        timestamp = Sys.time(),
        steps = character()
      )
    )

    class(pipe) <- "data_link_pipe"

    # -------------------------------------------------------
    # 5. Log do step
    # -------------------------------------------------------
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "proc_core")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "proc_core")

    pipe$proc_meta$normalize <- list(
      removed_suffix = TRUE,
      timestamp = Sys.time()
    )

    # -------------------------------------------------------
    # 6. Retorno
    # -------------------------------------------------------
    return(pipe)
  }
}
