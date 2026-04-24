# =========================
# STEP: REPORT TABLE 1
# =========================

step_report_table1 <- function() {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    if (!is.data.frame(df)) {
      stop("`report$data$raw` deve ser um data.frame.")
    }

    idioma <- report$meta$idioma %||% "pt"
    var.by <- report$meta$var.by %||% NULL

    # -------------------------------------------------------
    # Helpers
    # -------------------------------------------------------
    fmt_pct <- function(n, denom) {
      if (is.na(denom) || denom == 0) return(NA_character_)
      paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
    }

    fmt_num <- function(x) {
      if (length(x) == 0 || all(is.na(x))) return(NA_character_)
      x <- suppressWarnings(as.numeric(x))
      if (all(is.na(x))) return(NA_character_)
      med <- stats::median(x, na.rm = TRUE)
      q1  <- stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)
      q3  <- stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)
      paste0(
        formatC(med, format = "f", digits = 1),
        " (",
        formatC(q1, format = "f", digits = 1),
        " - ",
        formatC(q3, format = "f", digits = 1),
        ")"
      )
    }

    format_p <- function(p) {
      if (is.na(p)) return(NA_character_)
      if (p < 0.001) return("<0.001")
      formatC(p, format = "f", digits = 3)
    }

    safe_chisq_p <- function(x, g) {
      x <- as.factor(x)
      g <- as.factor(g)
      ok <- stats::complete.cases(x, g)
      x <- x[ok]
      g <- g[ok]

      if (length(unique(g)) < 2 || length(unique(x)) < 2) return(NA_real_)

      tab <- table(x, g)
      out <- tryCatch({
        suppressWarnings(stats::chisq.test(tab)$p.value)
      }, error = function(e) {
        NA_real_
      })
      out
    }

    safe_kw_p <- function(x, g) {
      ok <- stats::complete.cases(x, g)
      x <- suppressWarnings(as.numeric(x[ok]))
      g <- as.factor(g[ok])

      if (length(unique(g)) < 2 || sum(!is.na(x)) < 2) return(NA_real_)

      out <- tryCatch({
        stats::kruskal.test(x ~ g)$p.value
      }, error = function(e) {
        NA_real_
      })
      out
    }

    # -------------------------------------------------------
    # Variáveis que vão compor a Tabela 1
    # -------------------------------------------------------
    vars_cat <- c(
      "Sexo",
      "Raca_Cor",
      "Escolaridade_Violencia",
      "Escolaridade_Obito",
      "SGM",
      "Deficiencia",
      "Agressao_Familia",
      "Agressao_Par_Intimo",
      "Agressao_Comunitaria",
      "Violencia_Fisica_Ampla",
      "Violencia_Sexual",
      "Lesao_Autoprovocada",
      "Morte_Violenta"
    )

    vars_num <- c(
      "Idade_Violencia",
      "Idade_Obito",
      "Tempo_Sobrevivencia",
      "APVP",
      "AVCI"
    )

    # Manter apenas variáveis presentes
    vars_cat <- intersect(vars_cat, names(df))
    vars_num <- intersect(vars_num, names(df))

    # -------------------------------------------------------
    # Estratificação por desfecho, se existir
    # -------------------------------------------------------
    has_outcome <- !is.null(var.by) && var.by %in% names(df)
    outcome_levels <- NULL

    if (has_outcome) {
      g <- as.character(df[[var.by]])
      g[is.na(g) | trimws(g) == ""] <- "Ausente"
      outcome_levels <- sort(unique(g))
    }

    # -------------------------------------------------------
    # Função para montar bloco categórico
    # -------------------------------------------------------
    make_cat_block <- function(var) {

      x <- as.character(df[[var]])
      x[is.na(x) | trimws(x) == ""] <- "Ausente"

      total_n <- length(x)
      lvls <- sort(unique(x))

      out_list <- lapply(lvls, function(lv) {

        row <- data.frame(
          Variavel = var,
          Categoria = lv,
          Total = fmt_pct(sum(x == lv), total_n),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        if (has_outcome) {
          g <- as.character(df[[var.by]])
          g[is.na(g) | trimws(g) == ""] <- "Ausente"

          for (gl in outcome_levels) {
            denom <- sum(g == gl)
            cnt <- sum(x == lv & g == gl)
            row[[gl]] <- fmt_pct(cnt, denom)
          }

          p <- safe_chisq_p(x, g)
          row$`p.valor` <- format_p(p)
        }

        row
      })

      dplyr::bind_rows(out_list)
    }

    # -------------------------------------------------------
    # Função para montar bloco numérico
    # -------------------------------------------------------
    make_num_block <- function(var) {

      x <- suppressWarnings(as.numeric(df[[var]]))

      row <- data.frame(
        Variavel = var,
        Categoria = "Mediana (IIQ)",
        Total = fmt_num(x),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (has_outcome) {
        g <- as.character(df[[var.by]])
        g[is.na(g) | trimws(g) == ""] <- "Ausente"

        for (gl in outcome_levels) {
          row[[gl]] <- fmt_num(x[g == gl])
        }

        p <- safe_kw_p(x, g)
        row$`p.valor` <- format_p(p)
      }

      row
    }

    # -------------------------------------------------------
    # Construir tabela final
    # -------------------------------------------------------
    tab_cat <- if (length(vars_cat) > 0) {
      dplyr::bind_rows(lapply(vars_cat, make_cat_block))
    } else {
      NULL
    }

    tab_num <- if (length(vars_num) > 0) {
      dplyr::bind_rows(lapply(vars_num, make_num_block))
    } else {
      NULL
    }

    tabela1 <- dplyr::bind_rows(tab_cat, tab_num)

    # Reordenar colunas
    if (has_outcome) {
      tabela1 <- tabela1[, c(
        "Variavel",
        "Categoria",
        "Total",
        outcome_levels,
        "p.valor"
      ), drop = FALSE]
    } else {
      tabela1 <- tabela1[, c(
        "Variavel",
        "Categoria",
        "Total"
      ), drop = FALSE]
    }

    # -------------------------------------------------------
    # Texto breve
    # -------------------------------------------------------
    if (idioma == "pt") {
      texto1 <- paste0(
        "A Tabela 1 apresenta a caracterização das vítimas, das violências e dos óbitos ",
        if (has_outcome) paste0("segundo o desfecho definido por `", var.by, "`.") else "de forma geral.",
        " As variáveis categóricas são apresentadas em frequência e percentual, ",
        "e as variáveis numéricas em mediana e intervalo interquartil."
      )
    } else {
      texto1 <- paste0(
        "Table 1 presents the characterization of victims, violence and deaths ",
        if (has_outcome) paste0("according to the outcome defined by `", var.by, "`.") else "overall.",
        " Categorical variables are shown as frequency and percentage, ",
        "and numeric variables as median and interquartile range."
      )
    }

    # -------------------------------------------------------
    # Salvar no objeto
    # -------------------------------------------------------
    report$tables$t1 <- tabela1
    report$text$t1 <- texto1

    report$steps <- c(report$steps, "table1")
    report$log$table1 <- list(
      timestamp = Sys.time(),
      var.by = var.by,
      has_outcome = has_outcome,
      n_rows = nrow(tabela1)
    )

    return(report)
  }
}
