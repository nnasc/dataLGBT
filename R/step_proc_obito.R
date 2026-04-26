# =========================
# STEP: OBITO
# =========================

step_proc_obito <- function() {

  function(pipe) {

    # -------------------------
    # 1. Validação
    # -------------------------
    if (!inherits(pipe, "data_link_pipe") ||
        is.null(pipe$data$proc) ||
        is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_obito()`.", call. = FALSE)
    }

    df <- pipe$data$proc$data

    required_vars <- c("NUMERODO", "DTOBITO", "CIRCOBITO", "CAUSABAS")
    missing_vars <- setdiff(required_vars, names(df))

    if (length(missing_vars) > 0) {
      stop(
        paste0(
          "Variaveis ausentes para `step_proc_obito()`: ",
          paste(missing_vars, collapse = ", ")
        ),
        call. = FALSE
      )
    }

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

    parse_date_br <- function(x) {

      if (inherits(x, "Date")) return(x)

      x <- as.character(x)
      x <- trimws(x)
      x[x == ""] <- NA_character_

      # Corrige datas com 7 dígitos (ex: 1012020 → 01012020)
      idx7 <- !is.na(x) & nchar(x) == 7
      x[idx7] <- paste0("0", x[idx7])

      out <- suppressWarnings(as.Date(x, format = "%d%m%Y"))

      alt <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
      out[is.na(out) & !is.na(alt)] <- alt[is.na(out) & !is.na(alt)]

      return(out)
    }

    binario_sim_nao <- function(x) {
      dplyr::case_when(
        to_int(x) == 1 ~ "1 - Sim",
        to_int(x) == 2 ~ "0 - Nao",
        to_int(x) == 9 ~ NA_character_,
        TRUE ~ NA_character_
      )
    }

    # -------------------------
    # 4. Variáveis do óbito
    # -------------------------
    df <- dplyr::mutate(
      df,

      Tipo_Obito = dplyr::case_when(
        to_int(TIPOBITO) == 1 ~ "Fetal",
        to_int(TIPOBITO) == 2 ~ "Nao fetal",
        TRUE ~ NA_character_
      ),

      Data_Obito = parse_date_br(DTOBITO),

      Hora_Obito = dplyr::if_else(
        !is.na(HORAOBITO),
        as.character(HORAOBITO),
        NA_character_
      ),

      Local_Obito = dplyr::case_when(
        to_int(LOCOCOR) == 1 ~ "Hospital",
        to_int(LOCOCOR) == 2 ~ "Outros estabelecimentos de saude",
        to_int(LOCOCOR) == 3 ~ "Domicilio",
        to_int(LOCOCOR) == 4 ~ "Via publica",
        to_int(LOCOCOR) == 5 ~ "Outros",
        TRUE ~ NA_character_
      ),

      Circunstancia_Obito = dplyr::case_when(
        to_int(CIRCOBITO) == 1 ~ "Acidente",
        to_int(CIRCOBITO) == 2 ~ "Suicidio",
        to_int(CIRCOBITO) == 3 ~ "Homicidio",
        to_int(CIRCOBITO) == 4 ~ "Outros",
        TRUE ~ NA_character_
      ),

      Morte_Violenta = dplyr::case_when(
        to_int(CIRCOBITO) %in% c(1, 2, 3, 4) ~ "1 - Sim",
        TRUE ~ "0 - Nao"
      ),

      Assistencia_Medica = binario_sim_nao(ASSISTMED),
      Exame_Complementar = binario_sim_nao(EXAME),
      Cirurgia = binario_sim_nao(CIRURGIA),
      Necropsia = binario_sim_nao(NECROPSIA),
      Obito_Investigado = binario_sim_nao(TPPOS),
      Acidente_Trabalho = binario_sim_nao(ACIDTRAB),

      Fonte_Informacao = dplyr::case_when(
        to_int(FONTE) == 1 ~ "Boletim de ocorrencia",
        to_int(FONTE) == 2 ~ "Hospital",
        to_int(FONTE) == 3 ~ "Familia",
        to_int(FONTE) == 4 ~ "Outra",
        TRUE ~ NA_character_
      ),

      Fonte_Investigacao = dplyr::case_when(
        to_int(FONTEINV) == 1 ~ "Comite de morte materna e/ou infantil",
        to_int(FONTEINV) == 2 ~ "Visita domiciliar / Entrevista familia",
        to_int(FONTEINV) == 3 ~ "Estabelecimento de saude / Prontuario",
        to_int(FONTEINV) == 4 ~ "Relacionada com outros bancos de dados",
        to_int(FONTEINV) == 5 ~ "SVO",
        to_int(FONTEINV) == 6 ~ "IML",
        to_int(FONTEINV) == 7 ~ "Outra fonte",
        to_int(FONTEINV) == 8 ~ "Multiplas fontes",
        TRUE ~ NA_character_
      ),

      Causa_Basica = as.character(CAUSABAS),

      CID10_3 = dplyr::if_else(
        !is.na(CAUSABAS) & nchar(CAUSABAS) >= 3,
        substr(CAUSABAS, 1, 3),
        NA_character_
      )
    )

    # -------------------------
    # 5. Remover campos brutos
    # -------------------------
    remove_vars <- c(
      "NUMERODO", "TIPOBITO", "DTOBITO", "HORAOBITO", "LOCOCOR",
      "CIRCOBITO", "ASSISTMED", "EXAME", "CIRURGIA", "NECROPSIA",
      "TPPOS", "ACIDTRAB", "FONTE", "FONTEINV", "DTCADASTRO",
      "DTRECEBIM", "DTRECORIG", "CAUSABAS"
    )

    df <- dplyr::select(df, -dplyr::any_of(remove_vars))

    # -------------------------
    # 6. Atualizar pipeline
    # -------------------------
    pipe$data$proc$data <- df
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "obito")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "obito")

    return(pipe)
  }
}
