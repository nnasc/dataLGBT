data_link_validate <- function(x,
                               role = c("sinan", "sim"),
                               required_cols = NULL) {

  # -------------------------
  # Definir papel do objeto
  # -------------------------
  role <- match.arg(role)

  # -------------------------
  # Colunas obrigatórias padrão por base
  # -------------------------
  default_required_cols <- list(
    sinan = c("NM_PACIENT", "DT_NASC", "NM_MAE_PAC", "ORIENT_SEX", "IDENT_GEN", "CS_SEXO"),
    sim   = c("NOME", "DTNASC", "NOMEMAE", "CAUSABAS")
  )

  # -------------------------
  # Usar colunas padrão se o usuário não informar
  # -------------------------
  if (is.null(required_cols)) {
    required_cols <- default_required_cols[[role]]
  }

  # -------------------------
  # Validar required_cols
  # -------------------------
  if (!is.character(required_cols) || length(required_cols) == 0) {
    stop("`required_cols` deve ser um vetor de caracteres não vazio.")
  }

  # -------------------------
  # Normalizar entrada: aceitar 1 data.frame ou lista de data.frames
  # -------------------------
  if (is.data.frame(x)) {
    x <- list(x)
  } else if (!is.list(x)) {
    stop(
      paste0(
        "`x` deve ser um data.frame ou uma lista de data.frames para a base ",
        role, "."
      )
    )
  }

  # -------------------------
  # Lista vazia não é permitida
  # -------------------------
  if (length(x) == 0) {
    stop(paste0("A base ", role, " não pode ser uma lista vazia."))
  }

  # -------------------------
  # Validar cada elemento da lista
  # -------------------------
  for (i in seq_along(x)) {

    df <- x[[i]]

    # Cada elemento precisa ser data.frame
    if (!is.data.frame(df)) {
      stop(
        paste0(
          "Todos os elementos de `", role, "` devem ser data.frames. ",
          "Problema encontrado em `", role, "[[", i, "]]`."
        )
      )
    }

    # Nomes de colunas não podem ser NULL
    if (is.null(names(df)) || any(names(df) == "")) {
      stop(
        paste0(
          "`", role, "[[", i, "]]` possui nomes de colunas ausentes ou vazios."
        )
      )
    }

    # Colunas duplicadas não são permitidas
    if (any(duplicated(names(df)))) {
      stop(
        paste0(
          "`", role, "[[", i, "]]` possui colunas duplicadas."
        )
      )
    }

    # Verificar colunas obrigatórias
    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "`", role, "[[", i, "]]` não contém as colunas obrigatórias: ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
  }

  # -------------------------
  # Criar objeto validado com classe própria
  # -------------------------
  structure(
    x,
    class = c("validated_data_link", paste0("validated_", role)),
    role = role,
    required_cols = required_cols
  )
}
