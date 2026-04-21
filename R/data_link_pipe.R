data_link_pipe <- function(df_sinan, df_sim) {

  # -------------------------
  # 1. Checar se são objetos validados
  # -------------------------
  if (!inherits(df_sinan, "validated_sinan")) {
    stop("`df_sinan` deve ser validado com `data_link_validate(..., role = 'sinan')`.")
  }

  if (!inherits(df_sim, "validated_sim")) {
    stop("`df_sim` deve ser validado com `data_link_validate(..., role = 'sim')`.")
  }

  # -------------------------
  # 2. Garantir que são listas (padronização interna)
  # -------------------------
  # (a validação já garante isso, mas aqui reforça a consistência)
  sinan_list <- unclass(df_sinan)
  sim_list   <- unclass(df_sim)

  if (!all(purrr::map_lgl(sinan_list, is.data.frame))) {
    stop("`df_sinan` contém elementos que não são data.frames após validação.")
  }

  # -------------------------
  # 3. Unificar múltiplos data.frames (se houver)
  # -------------------------
  sinan_df <- dplyr::bind_rows(sinan_list, .id = "source_sinan")
  sim_df   <- dplyr::bind_rows(sim_list, .id = "source_sim")

  # -------------------------
  # 4. Criar objeto pipeline
  # -------------------------
  pipe <- structure(
    list(

      # ---------------------
      # DATA: estado atual
      # ---------------------
      data = list(
        sinan_raw = sinan_df,  # dados originais unificados
        sim_raw   = sim_df
      ),

      # ---------------------
      # LOG: auditoria por etapa
      # ---------------------
      log = list(
        input = list(
          sinan_n = nrow(sinan_df),
          sim_n   = nrow(sim_df)
        )
      ),

      # ---------------------
      # STEPS: histórico
      # ---------------------
      steps = "input",

      # ---------------------
      # META: metadados úteis
      # ---------------------
      meta = list(
        created_at = Sys.time(),
        sinan_sources = length(sinan_list),
        sim_sources   = length(sim_list)
      )

    ),
    class = "data_link_pipe"
  )

  return(pipe)
}
