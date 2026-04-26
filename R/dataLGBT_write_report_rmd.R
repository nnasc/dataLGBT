.dataLGBT_write_report_rmd <- function(report,
                                       output_file = "boletim_dataLGBT.pdf",
                                       logo_path = NULL,
                                       instituicao = "Ministério da Saúde",
                                       autores = "Equipe dataLGBT") {

  if (!inherits(report, "dataLGBT_report")) {
    stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
  }

  idioma <- report$meta$idioma %||% "pt"
  data_relatorio <- format(Sys.Date(), "%d/%m/%Y")

  # ---------------- LOGO ----------------
  if (is.null(logo_path) || !file.exists(logo_path)) {
    logo_path <- system.file("extdata", "logo.png", package = "dataLGBT")
  }

  if (!file.exists(logo_path)) {
    logo_chunk <- ""
  } else {
    logo_path <- normalizePath(logo_path, winslash = "/")
    logo_chunk <- paste0(
      "```{r, echo=FALSE, out.width='120px', fig.align='center'}\n",
      "knitr::include_graphics('", logo_path, "')\n",
      "```\n"
    )
  }

  # ---------------- YAML ----------------
  yaml <- paste0(
    "---
title: \"Boletim Epidemiológico\"
output:
  pdf_document:
    toc: false
fontsize: 11pt
geometry: margin=1in
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{graphicx}
---

")

  # ---------------- CAPA ----------------
  capa <- paste0(
    logo_chunk, "\n",
    "\\begin{center}\n",
    "\\Large\\textbf{Boletim Epidemiológico}\\\\[0.3cm]\n",
    "\\normalsize\\textbf{Violência e Mortalidade em Populações SGM}\\\\[0.3cm]\n",
    instituicao, "\\\\\n",
    "Data: ", data_relatorio, "\\\\\n",
    "Autores: ", autores, "\n",
    "\\end{center}\n\n",
    "\\vspace{0.5cm}\n"
  )

  # ---------------- SETUP ----------------
  setup <- "
```{r setup, include=FALSE}
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
```
"

  # ---------------- PRINCIPAIS ACHADOS ----------------
  gerar_achados <- function(report) {

    tryCatch({

      if (is.null(report$tables$t2)) {
        return("- Não há dados suficientes para gerar os principais achados.")
      }

      t2 <- report$tables$t2

      top_grupo <- t2$SGM[which.max(t2$Obitos)]
      total_obitos <- sum(t2$Obitos, na.rm = TRUE)
      total_casos <- sum(t2$Casos, na.rm = TRUE)

      achados <- c(
        paste0("Foram identificados ", total_casos, " casos e ", total_obitos, " óbitos no período analisado."),
        paste0("O grupo com maior número de óbitos foi: ", top_grupo, "."),
        "Observa-se heterogeneidade importante na distribuição das causas de morte.",
        "A carga de doença evidencia impacto relevante da violência na expectativa de vida."
      )

      paste0("- ", achados, collapse = "\n")

    }, error = function(e) {
      "- Não foi possível gerar automaticamente os principais achados."
    })
  }

  bloco_achados <- paste0(
    "\n# Principais Achados\n\n",
    gerar_achados(report), "\n\n"
  )

  # ---------------- HELPERS ----------------
  section_table <- function(id, titulo, texto) {
    paste0(
      "\n# ", titulo, "\n\n",
      if (!is.null(texto)) paste0(texto, "\n\n") else "",
      "```{r, echo=FALSE}\n",
      "if (!is.null(report$tables$", id, ")) {\n",
      " knitr::kable(report$tables$", id, ", format='latex', booktabs=TRUE, longtable=TRUE)\n",
      "}\n",
      "```\n"
    )
  }

  section_plot <- function(id, titulo, texto) {
    paste0(
      "\n# ", titulo, "\n\n",
      if (!is.null(texto)) paste0(texto, "\n\n") else "",
      "```{r, echo=FALSE, fig.width=7, fig.height=4.5}\n",
      "if (!is.null(report$graphs$", id, ")) {\n",
      " print(report$graphs$", id, ")\n",
      "}\n",
      "```\n"
    )
  }

  # ---------------- OVERVIEW ----------------
  bloco_overview <- paste0(
    "\n# Visão Geral\n\n",
    if (!is.null(report$text$overview)) paste0(report$text$overview, "\n\n") else "",
    "```{r, echo=FALSE, fig.width=7, fig.height=4.5}\n",
    "if (!is.null(report$graphs$g_overview)) print(report$graphs$g_overview)\n",
    "```\n",
    "```{r, echo=FALSE}\n",
    "if (!is.null(report$tables$overview)) knitr::kable(report$tables$overview, format='latex', booktabs=TRUE)\n",
    "```\n"
  )

  # ---------------- CONTEÚDO ----------------
  content <- paste0(
    bloco_overview,
    bloco_achados,

    section_table("t1", "Tabela 1. Caracterização dos Casos", report$text$t1),
    section_table("t2", "Tabela 2. Indicadores por SGM", report$text$t2),
    section_table("t3", "Tabela 3. Carga de Doença", report$text$t3),
    section_table("t4", "Tabela 4. Causas de Mortalidade", report$text$t4),

    section_plot("g1", "Gráfico 1. Óbitos no tempo", report$text$g1),
    section_plot("g2", "Gráfico 2. Casos no tempo", report$text$g2),
    section_plot("g3", "Gráfico 3. Letalidade por SGM", report$text$g3),
    section_plot("g4", "Gráfico 4. Causas de morte", report$text$g4),
    section_plot("g5", "Gráfico 5. Causas por SGM", report$text$g5)
  )

  # ---------------- WRITE ----------------
  rmd_file <- tempfile(fileext = ".Rmd")

  writeLines(c(yaml, capa, setup, content), rmd_file)

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # ---------------- RENDER ----------------
  rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  message("Boletim gerado em: ", normalizePath(output_file))

  invisible(output_file)
}
