# =========================
# STEP: AGRESSAO
# =========================

step_proc_agressao <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validacao
    # -------------------------
    if (!inherits(pipe, "data_link_pipe") ||
        is.null(pipe$data$proc) ||
        is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_agressao()`.", call. = FALSE)
    }

    df <- pipe$data$proc$data

    # -------------------------
    # 2. Garantir tipos
    # -------------------------
    df <- dplyr::mutate(
      df,
      dplyr::across(where(is.factor), as.character)
    )

    # -------------------------
    # 3. Helpers locais
    # -------------------------
    to_int <- function(x) {
      suppressWarnings(as.integer(as.character(x)))
    }

    binario_sim_nao <- function(x) {
      dplyr::case_when(
        to_int(x) == 1 ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      )
    }

    cat_na <- function(x) {
      dplyr::case_when(
        to_int(x) == 1 ~ "1 - Sim",
        to_int(x) == 2 ~ "0 - Nao",
        TRUE ~ NA_character_
      )
    }

    # -------------------------
    # 4. Meios de agressao - individuais
    # -------------------------
    df <- dplyr::mutate(
      df,

      Meio_Forca_Corporal = binario_sim_nao(AG_FORCA),
      Meio_Enforcamento = binario_sim_nao(AG_ENFOR),
      Meio_Objeto_Contundente = binario_sim_nao(AG_OBJETO),
      Meio_Objeto_PerfuroCortante = binario_sim_nao(AG_CORTE),
      Meio_Substancia_Objeto_Quente = binario_sim_nao(AG_QUENTE),
      Meio_Envenenamento = binario_sim_nao(AG_ENVEN),
      Meio_Arma_Fogo = binario_sim_nao(AG_FOGO),
      Meio_Ameaca = binario_sim_nao(AG_AMEACA),
      Meio_Outro = binario_sim_nao(AG_OUTROS),

      Meio_Outro_Especificar = dplyr::if_else(
        to_int(AG_OUTROS) == 1,
        as.character(AG_ESPEC),
        NA_character_
      )
    )

    # -------------------------
    # 5. Agressores - individuais
    # -------------------------
    df <- dplyr::mutate(
      df,

      Agressor_Pai = binario_sim_nao(REL_PAI),
      Agressor_Mae = binario_sim_nao(REL_MAE),
      Agressor_Padrasto = binario_sim_nao(REL_PAD),
      Agressor_Madrasta = binario_sim_nao(REL_MAD),
      Agressor_Conjuge = binario_sim_nao(REL_CONJ),
      Agressor_ExConjuge = binario_sim_nao(REL_EXCON),
      Agressor_Namorado = binario_sim_nao(REL_NAMO),
      Agressor_ExNamorado = binario_sim_nao(REL_EXNAM),
      Agressor_Filho = binario_sim_nao(REL_FILHO),
      Agressor_Irmao = binario_sim_nao(REL_IRMAO),
      Agressor_Conhecido = binario_sim_nao(REL_CONHEC),
      Agressor_Desconhecido = binario_sim_nao(REL_DESCO),
      Agressor_Cuidador = binario_sim_nao(REL_CUIDA),
      Agressor_Patrao = binario_sim_nao(REL_PATRAO),
      Agressor_Institucional = binario_sim_nao(REL_INST),
      Agressor_Policia = binario_sim_nao(REL_POL),
      Agressor_Outro = binario_sim_nao(REL_OUTROS),

      Agressor_Outro_Especificar = dplyr::if_else(
        to_int(REL_OUTROS) == 1,
        as.character(REL_ESPEC),
        NA_character_
      )
    )

    # -------------------------
    # 6. Variaveis descritivas do agressor
    # -------------------------
    df <- dplyr::mutate(
      df,

      Numero_Agressores = dplyr::case_when(
        to_int(NUM_ENVOLV) == 1 ~ "1 - Apenas um agressor",
        to_int(AUTOR_SEXO) == 2 ~ "2 - Dois ou mais agressores",
        TRUE ~ NA_character_
      ),

      Ciclo_Vida_Agressor = dplyr::case_when(
        to_int(CICL_VID) == 1 ~ "1 - Crianca",
        to_int(CICL_VID) == 2 ~ "2 - Adolescente",
        to_int(CICL_VID) == 3 ~ "3 - Jovem",
        to_int(CICL_VID) == 4 ~ "4 - Adulto",
        to_int(CICL_VID) == 5 ~ "5 - Idoso",
        TRUE ~ NA_character_
      ),

      Sexo_Agressor = dplyr::case_when(
        to_int(AUTOR_SEXO) == 1 ~ "1 - Masculino",
        to_int(AUTOR_SEXO) == 2 ~ "2 - Feminino",
        to_int(AUTOR_SEXO) == 3 ~ "3 - Ambos",
        TRUE ~ NA_character_
      ),

      Suspeita_Alcool_Agressor = cat_na(AUTOR_ALCO)
    )

    # -------------------------
    # 7. Variaveis resumidas
    # -------------------------
    df <- dplyr::mutate(
      df,

      Agressao_Familia = dplyr::case_when(
        Agressor_Pai == "1 - Sim" ~ "1 - Sim",
        Agressor_Mae == "1 - Sim" ~ "1 - Sim",
        Agressor_Padrasto == "1 - Sim" ~ "1 - Sim",
        Agressor_Madrasta == "1 - Sim" ~ "1 - Sim",
        Agressor_Filho == "1 - Sim" ~ "1 - Sim",
        Agressor_Irmao == "1 - Sim" ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      ),

      Agressao_Par_Intimo = dplyr::case_when(
        Agressor_Conjuge == "1 - Sim" ~ "1 - Sim",
        Agressor_ExConjuge == "1 - Sim" ~ "1 - Sim",
        Agressor_Namorado == "1 - Sim" ~ "1 - Sim",
        Agressor_ExNamorado == "1 - Sim" ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      ),

      Agressao_Comunitaria = dplyr::case_when(
        Agressor_Conhecido == "1 - Sim" ~ "1 - Sim",
        Agressor_Desconhecido == "1 - Sim" ~ "1 - Sim",
        Agressor_Cuidador == "1 - Sim" ~ "1 - Sim",
        Agressor_Patrao == "1 - Sim" ~ "1 - Sim",
        Agressor_Institucional == "1 - Sim" ~ "1 - Sim",
        Agressor_Policia == "1 - Sim" ~ "1 - Sim",
        Agressor_Outro == "1 - Sim" ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      ),

      Meio_Sem_Objetos = dplyr::case_when(
        Meio_Forca_Corporal == "1 - Sim" ~ "1 - Sim",
        Meio_Enforcamento == "1 - Sim" ~ "1 - Sim",
        Meio_Ameaca == "1 - Sim" ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      ),

      Meio_Objetos_Perfuro_Substancias = dplyr::case_when(
        Meio_Objeto_Contundente == "1 - Sim" ~ "1 - Sim",
        Meio_Objeto_PerfuroCortante == "1 - Sim" ~ "1 - Sim",
        Meio_Substancia_Objeto_Quente == "1 - Sim" ~ "1 - Sim",
        Meio_Envenenamento == "1 - Sim" ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      )
    )

    # -------------------------
    # 8. Remover proprio agressor como categoria
    # -------------------------
    remove_vars <- c("Agressor_Propria_Pessoa")
    df <- dplyr::select(df, -dplyr::any_of(remove_vars))

    # -------------------------
    # 9. Atualizar pipeline
    # -------------------------
    pipe$data$proc$data <- df
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "agressao")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "agressao")

    return(pipe)
  }
}
