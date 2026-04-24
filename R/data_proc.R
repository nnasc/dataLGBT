# =========================
# DATA PROC ENGINE
# =========================

data_proc <- function(link_obj,
                      expectativa_vida = 76.6,
                      usar_pesos_daly = TRUE) {

  # -------------------------
  # 0. Validação
  # -------------------------
  if (!inherits(link_obj, "data_link_pipe")) {
    stop("`data_proc()` requer um objeto do tipo 'data_link_pipe'. Execute `data_link()` primeiro.")
  }

  if (is.null(link_obj$data$linkage$matches)) {
    stop("Nenhum match encontrado. Execute `data_link()` corretamente antes.")
  }

  # -------------------------
  # 1. Inicializar estrutura
  # -------------------------
  proc <- list(
    data = list(),
    log  = list()
  )

  class(proc) <- "data_proc_pipe"

  # -------------------------
  # 2. Base inicial (sempre matches)
  # -------------------------
  base <- link_obj$data$linkage$matches

  if (!is.data.frame(base) || nrow(base) == 0) {
    stop("Objeto de matches inválido.")
  }

  proc$data$raw <- base

  # -------------------------
  # 3. Helper: pipeline interno
  # -------------------------
  pipe_add <- function(obj, step_name, step_fun) {
    obj$data[[step_name]] <- step_fun(obj$data)
    obj$log[[step_name]]  <- paste0("Executado em: ", Sys.time())
    return(obj)
  }

  # -------------------------
  # 4. Executar steps
  # -------------------------

  proc <- pipe_add(proc, "id", step_proc_id())

  proc <- pipe_add(proc, "sociodemografico", step_proc_sociodemo())

  proc <- pipe_add(proc, "violencia", step_proc_violence())

  proc <- pipe_add(proc, "agressao", step_proc_agressao())

  proc <- pipe_add(proc, "obito", step_proc_obito())

  proc <- pipe_add(
    proc,
    "cid",
    step_proc_cid(
      expectativa_vida = expectativa_vida,
      usar_pesos_daly = usar_pesos_daly
    )
  )

  # -------------------------
  # 5. Base final consolidada
  # -------------------------
  proc$data$final <- proc$data$cid

  # -------------------------
  # 6. Metadata
  # -------------------------
  proc$meta <- list(
    n_inicial = nrow(proc$data$raw),
    n_final   = nrow(proc$data$final),
    expectativa_vida = expectativa_vida,
    timestamp = Sys.time()
  )

  return(proc)
}
