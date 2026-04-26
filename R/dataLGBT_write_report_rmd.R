.dataLGBT_write_report_rmd <- function(report,
                                       output_file = "boletim_dataLGBT",
                                       logo_path = NULL,
                                       instituicao = "Ministério da Saúde",
                                       autores = "Equipe dataLGBT",
                                       format = c("pdf", "html", "both")) {

  format <- match.arg(format)

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

  # ---------------- YAML BASE ----------------
  yaml_base <- "---
title: \"Boletim Epidemiológico\"
fontsize: 11pt
geometry: margin=1in
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{graphicx}
---
"

  yaml_pdf <- "output:\n  pdf_document:\n    toc: false\n"
  yaml_html <- "output:\n  html_document:\n    toc: false\n"

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
{r setup, include=FALSE}
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
"

  # ---------------- CONTEÚDO ----------------
  content <- paste0(
    "\n# Visão Geral\n\n",
    "{r, echo=FALSE}\n",
    "if (!is.null(report$tables$overview)) knitr::kable(report$tables$overview)\n"
  )

  # ---------------- RMD TEMP ----------------
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(c(yaml_base, capa, setup, content), rmd_file)

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # ---------------- RENDER FUNÇÃO ----------------
  render_one <- function(fmt, ext) {

    out_file <- paste0(output_file, ".", ext)

    yaml <- if (fmt == "pdf") yaml_pdf else yaml_html

    tmp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(yaml_base, yaml, capa, setup, content), tmp_rmd)

    tryCatch({

      rmarkdown::render(
        input = tmp_rmd,
        output_file = out_file,
        envir = list2env(list(report = report), parent = globalenv()),
        quiet = TRUE
      )

      out_file

    }, error = function(e) {
      warning(paste("Falha ao gerar", fmt, ":", e$message))
      NULL
    })
  }

  # ---------------- EXECUÇÃO ----------------
  result <- list()

  if (format %in% c("pdf", "both")) {
    result$pdf <- render_one("pdf", "pdf")
  }

  if (format %in% c("html", "both")) {
    result$html <- render_one("html", "html")
  }

  if (is.null(result$pdf) && is.null(result$html)) {
    stop("Falha ao gerar PDF e HTML.")
  }

  message("Relatório gerado com sucesso.")

  invisible(result)
}
