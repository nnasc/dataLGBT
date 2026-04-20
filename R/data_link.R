#' Realiza vinculação (linkage) entre bases de dados
#'
#' Função para integrar dados provenientes de diferentes sistemas
#' de informação em saúde, como SINAN e SIM.
#'
#' @param df_sinan Data frame contendo dados do SINAN
#' @param df_sim Data frame contendo dados do SIM
#' @param threshold Valor numérico para critério de pareamento
#'
#' @return Um data frame com os registros vinculados
#' @export
data_link <- function(df_sinan,
                      df_sim,
                      dedup_sim = FALSE,
                      filter_sogi = c("all","lgbt","gay","lesbian",
                                      "bissexual","travesti",
                                      "mul.trans","hom.trans"),
                      threshold = 0.35,
                      save_summary = TRUE,
                      summary_path = "linkage_summary.txt"){

  # Função para validar os DataFrames -------------------------

  validate_and_prepare_df <- function(x, required_cols, arg_name) {

    # =====================
    # Validar required_cols
    # =====================
    if (!is.character(required_cols) || length(required_cols) == 0) {
      stop("`required_cols` deve ser um vetor de caracteres não vazio")
    }

    # =====================
    # Normalizar entrada
    # =====================
    if (is.data.frame(x)) {
      x <- list(x)
    } else if (!is.list(x)) {
      stop(
        paste0(
          "`", arg_name, "` deve ser um data.frame ou lista de data.frames.\n",
          "Exemplo: list(df1, df2)"
        )
      )
    }

    # =====================
    # Lista vazia
    # =====================
    if (length(x) == 0) {
      stop(paste0("`", arg_name, "` não pode ser uma lista vazia"))
    }

    # =====================
    # Validar conteúdo
    # =====================
    if (!all(purrr::map_lgl(x, is.data.frame))) {
      stop(
        paste0("Todos os elementos de `", arg_name, "` devem ser data.frames")
      )
    }

    # =====================
    # Validar cada dataset
    # =====================
    for (i in seq_along(x)) {

      df <- x[[i]]

      df_name <- names(x)[i]

      label <- if (!is.null(df_name) && df_name != "") {
        paste0(arg_name, "$", df_name)
      } else {
        paste0(arg_name, "[[", i, "]]")
      }

      if (!is.character(names(df))) {
        stop(paste0("`", label, "` possui nomes de colunas inválidos"))
      }

      if (any(duplicated(names(df)))) {
        stop(paste0("`", label, "` possui colunas duplicadas"))
      }

      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(
          paste0(
            "`", label, "` não contém as colunas obrigatórias:\n",
            paste(missing_cols, collapse = ", ")
          )
        )
      }
    }

    return(x)
  }

  req_cols_sinan <- c("NM_PACIENT","DT_NASC","NM_MAE_PAC",
                      "ORIENT_SEX","IDENT_GEN","CS_SEXO")

  df_sinan <- validate_and_prepare_df(
    df_sinan,
    required_cols = req_cols_sinan,
    arg_name = "df_sinan"
  )

  req_cols_sim <- c("NOME","DTNASC","NOMEMAE","CAUSABAS")

  df_sim <- validate_and_prepare_df(
    df_sim,
    required_cols = req_cols_sim,
    arg_name = "df_sim"
  )

  # Merging banco de dados  -------------------------

  df_sinan <- dplyr::bind_rows(df_sinan, .id = "source_sinan")
  df_sim  <- dplyr::bind_rows(df_sim, .id = "source_sim")

  # Filtrar por identidade LGBT  -------------------------

  # Filtro SOGI

  filter_sogi <- match.arg(filter_sogi)

  df_sinan <- df_sinan %>%
    dplyr::mutate(

      sogi_group = dplyr::case_when(

        # Gay
        ORIENT_SEX == 2 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          CS_SEXO == "M" ~ "gay",

        # Lésbica
        ORIENT_SEX == 2 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) &
          CS_SEXO == "F" ~ "lesbian",

        # Bissexual
        ORIENT_SEX == 3 &
          (IDENT_GEN %in% c(8, 9) | is.na(IDENT_GEN)) ~ "bissexual",

        # Travesti
        IDENT_GEN == 1 ~ "travesti",

        # Mulher trans
        IDENT_GEN == 2 ~ "mul.trans",

        # Homem trans
        IDENT_GEN == 3 ~ "hom.trans",

        # Não LGBT
        TRUE ~ "nao_lgbt"
      )
    )

  # Aplicar filtro

  if (filter_sogi == "all") {

    # Não filtra nada
    df_sinan <- df_sinan

  } else if (filter_sogi == "lgbt") {

    df_sinan <- df_sinan %>%
      dplyr::filter(sogi_group != "nao_lgbt")

  } else {

    df_sinan <- df_sinan %>%
      dplyr::filter(sogi_group == filter_sogi)
  }

  # Deduplicação SINAN (fastLink) ------------

  # Padronizar variáveis para linkage
  sinan_link <- df_sinan %>%
    rename("NOME" = "NM_PACIENT", "NOMEMAE" = "NM_MAE_PAC") %>%
    mutate(NOME = rm_accent(NOME),
           NOME = str_remove_all(NOME, fixed(".")),
           NOME = str_remove_all(NOME, fixed("-")),
           NOME = str_replace_all(NOME, " +", ""),
           NOME = toupper(NOME),
           NOMEMAE = rm_accent(NOMEMAE),
           NOMEMAE = toupper(NOMEMAE),
           NOMEMAE = str_remove_all(NOMEMAE, fixed(".")),
           NOMEMAE = str_remove_all(NOMEMAE, fixed("-")),
           NOMEMAE = str_replace_all(NOMEMAE, " +", ""),
           DT_NASC = str_remove_all(DT_NASC, "-"),
           DTNASC = paste0(str_sub(DT_NASC, 7, 8),
                           str_sub(DT_NASC, 5, 6),
                           str_sub(DT_NASC, 1, 4)))

  # Rodar fastLink (self-linkage)
  sinan_dedup <- fastLink::fastLink(
    dfA = sinan_link,
    dfB = sinan_link,
    varnames = c("NOME","DTNASC","NOMEMAE"),
    return.all = TRUE,
    verbose = FALSE
  )

  # Obter matches
  sinan_matches <- fastLink::getMatches(
    sinan_link,
    sinan_link,
    sinan_dedup
  )

  # Remover duplicados
  df_sinan_d <- sinan_matches %>%
    dplyr::distinct(dedupe.ids, .keep_all = TRUE)

  # Deduplicação SIM (fastLink) ------------

  if (isTRUE(dedup_sim)) {

    # Padronizar variáveis para linkage
    sim_link <- df_sim %>%
      mutate(NOME = rm_accent(NOME),
             NOME = toupper(NOME),
             NOME = str_remove_all(NOME, fixed(".")),
             NOME = str_remove_all(NOME, fixed("-")),
             NOME = str_replace_all(NOME, " +", ""),
             NOMEMAE = rm_accent(NOMEMAE),
             NOMEMAE = toupper(NOMEMAE),
             NOMEMAE = str_remove_all(NOMEMAE, fixed(".")),
             NOMEMAE = str_remove_all(NOMEMAE, fixed("-")),
             NOMEMAE = str_replace_all(NOMEMAE, " +", ""),
             DT_OBITO = as.Date(paste0(str_sub(DTOBITO, 5, 8),
                                       "-", str_sub(DTOBITO, 3, 4),
                                       "-", str_sub(DTOBITO, 1, 2))),
             DT_NASC = as.Date(paste0(str_sub(DTNASC, 5, 8),
                                      str_sub(DTNASC, 3, 4),
                                      str_sub(DTNASC, 1, 2))),
             DTNASC = as.character(DTNASC))

    sim_dedup <- fastLink::fastLink(
      dfA = sim_link,
      dfB = sim_link,
      varnames = c("NOME","DTNASC","NOMEMAE"),
      return.all = TRUE,
      verbose = TRUE
    )

    sim_matches <- fastLink::getMatches(
      sim_link,
      sim_link,
      sim_dedup
    )

    df_sim <- sim_matches %>%
      dplyr::distinct(dedupe.ids, .keep_all = TRUE)
  }

  data_linkage <- fastLink::fastLink(
    dfA = df_sinan,
    dfB = df_sim,
    varnames = c("NOME","DTNASC","NOMEMAE"),
    return.all = TRUE,
    verbose = TRUE
  )

  data_matches <- fastLink::getMatches(
    df_sinan,
    df_sim,
    data_linkage,
    threshold.match = threshold
  )


  # Auditoria do Linkage --------------

  {
    # Parâmetros esperados:
    # - data_linkage (objeto do fastLink)
    # - save_summary (TRUE/FALSE)
    # - summary_path (opcional, default abaixo)

    if (!exists("summary_path")) {
      summary_path <- "linkage_summary.txt"
    }

    # Captura toda a saída
    summary_output <- capture.output({

      cat("=====================================\n")
      cat("      RELATÓRIO DE LINKAGE\n")
      cat("=====================================\n\n")

      cat("Data:", format(Sys.time()), "\n\n")

      # ----------------------
      # Confusion Matrix
      # ----------------------
      cat("---- Confusion Matrix ----\n")
      tryCatch({
        print(fastLink::confusion(data_linkage))
      }, error = function(e) {
        cat("Erro em confusion():", e$message, "\n")
      })

      # ----------------------
      # Summary por thresholds
      # ----------------------
      cat("\n---- Summary por Threshold ----\n")
      tryCatch({
        print(summary(
          data_linkage,
          thresholds = c(0.99,0.95,0.90,0.85,0.75,
                         0.65,0.55,0.45,0.35,0.25,
                         0.10,0.05,0.01)
        ))
      }, error = function(e) {
        cat("Erro em summary():", e$message, "\n")
      })

      # ----------------------
      # Parâmetros EM
      # ----------------------
      cat("\n---- Parâmetros EM ----\n")
      if (!is.null(data_linkage$EM)) {
        tryCatch({
          fastLink::inspectEM(data_linkage$EM, digits = 2)
        }, error = function(e) {
          cat("Erro em inspectEM():", e$message, "\n")
        })
      } else {
        cat("Objeto EM não disponível.\n")
      }

      cat("\n=====================================\n")
    })

    # Imprime no console
    cat(paste(summary_output, collapse = "\n"))

    # Salva se solicitado
    if (isTRUE(save_summary)) {
      tryCatch({
        writeLines(summary_output, con = summary_path)
        message("Relatório salvo em: ", summary_path)
      }, error = function(e) {
        warning("Falha ao salvar relatório: ", e$message)
      })
    }

    invisible(summary_output)
  }

  # Output -------------
  return(list(
    data_matches = data_matches,
    summary = summary_output
  ))

}
