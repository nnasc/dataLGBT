# =========================
# STEP: REPORT TABLE 1
# =========================

step_report_table1 <- function(report) {

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
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 0 || all(is.na(x))) return(NA_character_)

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

      if (length(unique(x)) < 2 || length(unique(g)) < 2) return(NA_real_)

      tab <- table(x, g)

      tryCatch({
        suppressWarnings(stats::chisq.test(tab)$p.value)
      }, error = function(e) NA_real_)
    }

    safe_kw_p <- function(x, g) {
      ok <- stats::complete.cases(x, g)

      x <- suppressWarnings(as.numeric(x[ok]))
      g <- as.factor(g[ok])

      if (length(unique(g)) < 2 || sum(!is.na(x)) < 2) return(NA_real_)

      tryCatch({
        stats::kruskal.test(x ~ g)$p.value
      }, error = function(e) NA_real_)
    }

    # -------------------------------------------------------
    # Variáveis
    # -------------------------------------------------------
    vars_cat <- intersect(c(
      "Sexo","Raca_Cor","Escolaridade_Violencia","Escolaridade_Obito",
      "SGM","Deficiencia","Agressao_Familia","Agressao_Par_Intimo",
      "Agressao_Comunitaria","Violencia_Fisica_Ampla",
      "Violencia_Sexual","Lesao_Autoprovocada","Morte_Violenta"
    ), names(df))

    vars_num <- intersect(c(
      "Idade_Violencia","Idade_Obito","Tempo_Sobrevivencia","APVP","AVCI"
    ), names(df))

    # -------------------------------------------------------
    # Estratificação
    # -------------------------------------------------------
    has_outcome <- !is.null(var.by) && var.by %in% names(df)

    if (has_outcome) {
      g <- as.character(df[[var.by]])
      g[is.na(g) | trimws(g) == ""] <- "Ausente"
      outcome_levels <- sort(unique(g))
    }

    # -------------------------------------------------------
    # BLOCO CATEGÓRICO
    # -------------------------------------------------------
    make_cat_block <- function(var) {

      x <- as.character(df[[var]])
      x[is.na(x) | trimws(x) == ""] <- "Ausente"

      total_n <- length(x)
      lvls <- sort(unique(x))

      out <- lapply(lvls, function(lv) {

        row <- data.frame(
          Variavel = var,
          Categoria = lv,
          Total = fmt_pct(sum(x == lv), total_n),
          stringsAsFactors = FALSE
        )

        if (has_outcome) {

          g <- as.character(df[[var.by]])
          g[is.na(g) | trimws(g) == ""] <- "Ausente"

          for (gl in outcome_levels) {
            denom <- sum(g == gl)
            cnt <- sum(x == lv & g == gl)
            row[[gl]] <- fmt_pct(cnt, denom)
          }

          row$`p.valor` <- format_p(safe_chisq_p(x, g))
        }

        row
      })

      dplyr::bind_rows(out)
    }

    # -------------------------------------------------------
    # BLOCO NUMÉRICO
    # -------------------------------------------------------
    make_num_block <- function(var) {

      x <- suppressWarnings(as.numeric(df[[var]]))

      row <- data.frame(
        Variavel = var,
        Categoria = "Mediana (IIQ)",
        Total = fmt_num(x),
        stringsAsFactors = FALSE
      )

      if (has_outcome) {

        g <- as.character(df[[var.by]])
        g[is.na(g) | trimws(g) == ""] <- "Ausente"

        for (gl in outcome_levels) {
          row[[gl]] <- fmt_num(x[g == gl])
        }

        row$`p.valor` <- format_p(safe_kw_p(x, g))
      }

      row
    }

    # -------------------------------------------------------
    # Construção
    # -------------------------------------------------------
    tabela1 <- dplyr::bind_rows(
      if (length(vars_cat) > 0) dplyr::bind_rows(lapply(vars_cat, make_cat_block)),
      if (length(vars_num) > 0) dplyr::bind_rows(lapply(vars_num, make_num_block))
    )

    if (is.null(tabela1) || nrow(tabela1) == 0) {
      tabela1 <- NULL
    }

    # -------------------------------------------------------
    # Ordenação colunas
    # -------------------------------------------------------
    if (!is.null(tabela1)) {

      if (has_outcome) {
        tabela1 <- tabela1[, c(
          "Variavel","Categoria","Total",
          outcome_levels,"p.valor"
        ), drop = FALSE]
      } else {
        tabela1 <- tabela1[, c(
          "Variavel","Categoria","Total"
        ), drop = FALSE]
      }
    }

    # -------------------------------------------------------
    # Texto
    # -------------------------------------------------------
    if (idioma == "pt") {

      texto1 <- paste0(
        "A Tabela 1 apresenta a caracterização das vítimas, das violências e dos óbitos ",
        if (has_outcome) paste0("segundo o desfecho `", var.by, "`.") else "de forma geral.",
        " Variáveis categóricas são apresentadas como n (%), e variáveis numéricas como mediana (IIQ)."
      )

    } else {

      texto1 <- paste0(
        "Table 1 presents the characterization of victims, violence and deaths ",
        if (has_outcome) paste0("according to outcome `", var.by, "`.") else "overall.",
        " Categorical variables are shown as n (%), and numeric variables as median (IQR)."
      )
    }

    # -------------------------------------------------------
    # Save
    # -------------------------------------------------------
    report$tables$t1 <- tabela1
    report$text$t1 <- texto1

    report$steps <- c(report$steps, "table1")

    report$log$table1 <- list(
      timestamp = Sys.time(),
      var.by = var.by,
      has_outcome = has_outcome,
      n_rows = ifelse(is.null(tabela1), 0, nrow(tabela1))
    )

    return(report)
  }
