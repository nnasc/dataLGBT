# =========================
# STEP: PROC ID
# =========================

step_proc_id <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_id()`.")
    }

    df <- pipe$data$proc$data

    # -------------------------
    # 2. Criar IDs permitidos
    # -------------------------
    df <- dplyr::mutate(
      df,

      # Número da notificação de violência (SINAN)
      VReport_N = dplyr::coalesce(
        .data$NU_NOTIFIC.x,
        .data$NU_NOTIFIC
      ),

      # Número da Declaração de Óbito (SIM)
      DReport_N = dplyr::coalesce(
        .data$NUMERODO.y,
        .data$NUMERODO
      )
    )

    # -------------------------
    # 3. Remover identificadores sensíveis
    # -------------------------
    remove_vars <- c(
      # nomes
      "NOME", "NOME.x", "NOME.y",
      "NM_PACIENT", "NM_MAE_PAC",
      "NOMEMAE", "NOMEMAE.x", "NOMEMAE.y",

      # datas diretas identificáveis (mantemos idade depois)
      "DTNASC", "DTNASC.x", "DTNASC.y",
      "DT_NASC",

      # IDs internos / linkage
      ".id_sinan", ".id_sim", ".id_internal",
      ".block",
      "dedupe.ids",

      # chaves técnicas
      "KEY", "Unique_Key"
    )

    df <- dplyr::select(
      df,
      -dplyr::any_of(remove_vars)
    )

    # -------------------------
    # 4. Atualizar pipe
    # -------------------------
    pipe$data$proc$data <- df

    pipe$data$proc$steps <- c(
      pipe$data$proc$steps,
      "id"
    )

    pipe$proc_meta$steps <- c(
      pipe$proc_meta$steps,
      "id"
    )

    return(pipe)
  }
}
