.dataLGBT_write_report_qmd <- function(report,
                                       output_file = "boletim_dataLGBT",
                                       logo_path = NULL,
                                       instituicao = "Ministério da Saúde",
                                       autores = "Equipe dataLGBT",
                                       format = c("html", "pdf", "docx", "all")) {

  format <- match.arg(format)

  if (!inherits(report, "dataLGBT_report")) {
    stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
  }

  idioma <- report$meta$idioma %||% "pt"
  data_relatorio <- format(Sys.Date(), "%d/%m/%Y")

  # -------------------------------------------------------
  # LOGO
  # -------------------------------------------------------
  if (is.null(logo_path) || !file.exists(logo_path)) {
    logo_path <- system.file("extdata", "logo.png", package = "dataLGBT")
  }

  logo_block <- if (file.exists(logo_path)) {
    paste0("![](", normalizePath(logo_path, winslash = "/"), "){width=120px}\n")
  } else ""

  # -------------------------------------------------------
  # QMD HEADER
  # -------------------------------------------------------
  yaml <- paste0(
    "---\n",
    "title: \"Boletim Epidemiológico\"\n",
    "format:\n",
    "  html:\n",
    "    toc: false\n",
    "  pdf:\n",
    "    toc: false\n",
    "  docx:\n",
    "    toc: false\n",
    "execute:\n",
    "  echo: false\n",
    "---\n"
  )

  # -------------------------------------------------------
  # CONTENT (USANDO RENDER SAFE DATA)
  # -------------------------------------------------------
  content <- paste0(
    logo_block, "\n",

    "# Visão Geral\n\n",
    "```{r}\n",
    "report$data$render\n",
    "```\n",

    "\n# Tabela Overview\n\n",
    "```{r}\n",
    "report$tables$overview\n",
    "```\n",

    "\n# Gráfico\n\n",
    "```{r}\n",
    "report$graphs$g_overview\n",
    "```\n"
  )

  qmd_file <- tempfile(fileext = ".qmd")
  writeLines(c(yaml, content), qmd_file)

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # -------------------------------------------------------
  # RENDER SAFE (NUNCA ABORTA POR WARNING)
  # -------------------------------------------------------
  render_one <- function(fmt) {

    out <- paste0(output_file, ".", fmt)

    tryCatch({

      quarto::quarto_render(
        input = qmd_file,
        output_format = fmt,
        output_file = out
      )

      out

    }, warning = function(w) {
      message("Warning ignorado: ", w$message)
      invokeRestart("muffleWarning")
    }, error = function(e) {
      message("Erro no render (", fmt, "): ", e$message)
      NULL
    })
  }

  outputs <- list()

  if (format %in% c("html", "all")) outputs$html <- render_one("html")
  if (format %in% c("pdf", "all")) outputs$pdf <- render_one("pdf")
  if (format %in% c("docx", "all")) outputs$docx <- render_one("docx")

  if (all(vapply(outputs, is.null, logical(1)))) {
    stop("Falha em todos os formatos de saída.")
  }

  message("Relatório gerado via Quarto.")

  invisible(outputs)
}
