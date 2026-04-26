# =========================
# DATA PROC ENGINE
# =========================
#' @export
data_proc <- function(link_obj,
                      expectativa_vida = 76.6,
                      usar_pesos_daly = TRUE) {

  # -------------------------
  # 0. Validação
  # -------------------------
  if (!inherits(link_obj, "data_link_pipe")) {
    stop(
      "`data_proc()` requer um objeto do tipo 'data_link_pipe'. Execute `data_link()` primeiro.",
      call. = FALSE
    )
  }

  if (is.null(link_obj$data$linkage$matches) ||
      !is.data.frame(link_obj$data$linkage$matches) ||
      nrow(link_obj$data$linkage$matches) == 0) {
    stop("Nenhum match encontrado. Execute `data_link()` corretamente antes.", call. = FALSE)
  }

  # -------------------------
  # 1. Função auxiliar para executar steps
  # -------------------------
  run_step <- function(pipe, step_name, step_fun) {
    tryCatch(
      step_fun(pipe),
      error = function(e) {
        stop(
          sprintf("Falha no step '%s': %s", step_name, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  }

  # -------------------------
  # 2. Iniciar pipeline
  # -------------------------
  pipe <- link_obj

  pipe <- proc_core()(pipe)

  pipe <- run_step(pipe, "id", step_proc_id())
  pipe <- run_step(pipe, "sociodemografico", step_proc_sociodemo())
  pipe <- run_step(pipe, "violencia", step_proc_violence())
  pipe <- run_step(pipe, "agressao", step_proc_agressao())
  pipe <- run_step(pipe, "obito", step_proc_obito())
  pipe <- run_step(
    pipe,
    "cid",
    step_proc_cid(
      expectativa_vida = expectativa_vida,
      usar_pesos_daly = usar_pesos_daly
    )
  )

  # -------------------------
  # 3. Remover variáveis originais (AUTOMÁTICO)
  # -------------------------
  pipe <- run_step(pipe, "drop_raw", step_proc_drop_raw())

  # -------------------------
  # 4. Consolidar saída
  # -------------------------
  result <- list(
    data = pipe$data$proc$data,
    steps = pipe$data$proc$steps,
    meta = pipe$proc_meta,
    linkage = pipe$data$linkage
  )

  class(result) <- "data_proc_pipe"
  return(result)
}
