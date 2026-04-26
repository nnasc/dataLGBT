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

    # guarda os nomes antes da normalização
    raw_vars_original <- names(df)

    # -------------------------------------------------------
    # 3. NORMALIZAÇÃO DE NOMES (CRÍTICO)
    # -------------------------------------------------------
    normalize_names <- function(df) {

      nms <- names(df)

      nms <- gsub("\\.(x|y)$", "", nms)
      nms <- make.unique(nms)

      names(df) <- nms
      df
    }

    df <- normalize_names(df)

    # guarda os nomes já normalizados que serão removidos no fim
    raw_vars <- names(df)

    # -------------------------------------------------------
    # 4. Criar estrutura do pipe
    # -------------------------------------------------------
    pipe <- list(
      data = list(
        linkage = data_links$data$linkage,
        proc = list(
          data = df,
          steps = character()
        )
      ),
      proc_meta = list(
        timestamp = Sys.time(),
        steps = character(),
        raw_vars_original = raw_vars_original,
        raw_vars = raw_vars
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
