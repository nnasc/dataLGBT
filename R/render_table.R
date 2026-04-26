# =========================
# INTERNAL: TABLE RENDERERS
# =========================

.render_table <- function(name, obj, text = NULL) {

  if (is.null(obj)) return(NULL)

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Pacote 'kableExtra' é necessário para renderização das tabelas.")
  }

  if (name == "t1") {
    return(.render_table1(obj, text))
  }

  if (name %in% c("t2", "t3", "t4")) {
    return(.render_simple(obj, text, name))
  }

  knitr::kable(obj)
}

# =========================
# TABLE 1 (HIERÁRQUICA)
# =========================

.render_table1 <- function(df, text = NULL) {

  library(dplyr)

  df <- df %>%
    mutate(
      Variavel = ifelse(
        duplicated(Variavel),
        "",
        Variavel
      )
    )

  tab <- knitr::kable(
    df,
    align = paste0("ll", paste(rep("c", ncol(df) - 2), collapse = "")),
    caption = "Tabela 1. Caracterização da população"
  )

  tab <- kableExtra::kable_styling(
    tab,
    full_width = FALSE,
    position = "center"
  )

  if (!is.null(text)) {
    cat("\n\n", text, "\n\n")
  }

  tab
}

# =========================
# TABLES SIMPLES (T2, T3, T4)
# =========================

.render_simple <- function(df, text = NULL, name = NULL) {

  titulo <- switch(name,
                   t2 = "Tabela 2. Indicadores segundo SGM",
                   t3 = "Tabela 3. Carga de doença (DALY)",
                   t4 = "Tabela 4. Distribuição de óbitos por CID",
                   "Tabela")

  tab <- knitr::kable(
    df,
    align = paste(rep("c", ncol(df)), collapse = ""),
    caption = titulo
  )

  tab <- kableExtra::kable_styling(
    tab,
    full_width = FALSE,
    position = "center"
  )

  if (!is.null(text)) {
    cat("\n\n", text, "\n\n")
  }

  tab
}
