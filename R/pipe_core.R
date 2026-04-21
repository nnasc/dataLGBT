# -------------------------
# Helper interno seguro
# -------------------------
safe_nrow <- function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  } else {
    return(NA_integer_)
  }
}

# -------------------------
# Adicionar step ao pipeline
# -------------------------
pipe_add_step <- function(pipe, step_name, fun) {

  # -------------------------
  # 1. Validações
  # -------------------------
  if (!inherits(pipe, "data_link_pipe")) {
    stop("`pipe` deve ser um objeto da classe 'data_link_pipe'.")
  }

  if (!is.character(step_name) || length(step_name) != 1 || step_name == "") {
    stop("`step_name` deve ser uma string não vazia.")
  }

  if (!is.function(fun)) {
    stop("`fun` deve ser uma função.")
  }

  if (step_name %in% pipe$steps) {
    warning(
      paste0(
        "O step '", step_name,
        "' já foi executado anteriormente e será registrado novamente."
      )
    )
  }

  # -------------------------
  # 2. Estado anterior
  # -------------------------
  old_data <- pipe$data

  # -------------------------
  # 3. Aplicar transformação
  # -------------------------
  new_data <- fun(old_data)

  # -------------------------
  # 4. Validar saída
  # -------------------------
  if (!is.list(new_data)) {
    stop("A função do step deve retornar uma lista (`data`).")
  }

  required_names <- c("sinan_raw", "sim_raw")

  if (!all(required_names %in% names(new_data))) {
    stop(
      paste0(
        "O step retornou um objeto inválido. ",
        "Elementos obrigatórios ausentes: ",
        paste(setdiff(required_names, names(new_data)), collapse = ", ")
      )
    )
  }

  # -------------------------
  # 5. Atualizar estado
  # -------------------------
  pipe$data <- new_data
  pipe$steps <- c(pipe$steps, step_name)

  # -------------------------
  # 6. Logging base
  # -------------------------
  log_entry <- list(
    timestamp = Sys.time(),
    sinan_n_before = if (is.data.frame(old_data$sinan_raw)) nrow(old_data$sinan_raw) else NA_integer_,
    sim_n_before   = if (is.data.frame(old_data$sim_raw)) nrow(old_data$sim_raw) else NA_integer_,
    sinan_n_after  = if (is.data.frame(new_data$sinan_raw)) nrow(new_data$sinan_raw) else NA_integer_,
    sim_n_after    = if (is.data.frame(new_data$sim_raw)) nrow(new_data$sim_raw) else NA_integer_
  )

  # -------------------------
  # 7. Attach step-specific info, if provided
  # -------------------------
  step_info <- attr(new_data, "step_info")
  if (!is.null(step_info) && is.list(step_info)) {
    log_entry$step_info <- step_info
  }

  pipe$log[[step_name]] <- log_entry

  return(pipe)
}


# -------------------------
# Acesso controlado aos dados
# -------------------------
data_link_get <- function(pipe, name = NULL, all = FALSE) {

  # -------------------------
  # 1. Validação
  # -------------------------
  if (!inherits(pipe, "data_link_pipe")) {
    stop("`pipe` deve ser um objeto da classe 'data_link_pipe'.")
  }

  # -------------------------
  # 2. Retornar tudo (explícito)
  # -------------------------
  if (is.null(name)) {

    if (!isTRUE(all)) {
      stop(
        "Para retornar todo o conteúdo do pipe, use `all = TRUE`."
      )
    }

    return(pipe$data)
  }

  # -------------------------
  # 3. Validar nome
  # -------------------------
  if (!is.character(name) || length(name) != 1) {
    stop("`name` deve ser uma string única.")
  }

  if (!name %in% names(pipe$data)) {
    stop(
      paste0(
        "Elemento '", name, "' não encontrado em `pipe$data`. ",
        "Disponíveis: ", paste(names(pipe$data), collapse = ", ")
      )
    )
  }

  # -------------------------
  # 4. Retornar elemento
  # -------------------------
  return(pipe$data[[name]])
}
