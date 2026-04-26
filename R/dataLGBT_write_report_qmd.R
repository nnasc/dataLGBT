.dataLGBT_write_report_qmd <- function(report,
                                       output_file = "dataLGBT_report",
                                       format = c("html", "pdf", "docx", "all"),
                                       instituicao = "Ministério da Saúde",
                                       autores = "dataLGBT team",
                                       logo_path = NULL) {

  format <- match.arg(format)

  if (!inherits(report, "dataLGBT_report")) {
    stop("Objeto inválido: esperado dataLGBT_report")
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("O pacote 'quarto' precisa estar instalado.")
  }

  out_dir <- getwd()

  df <- report$data$render

  if (is.null(df) || !is.data.frame(df)) {
    stop("Render layer ausente")
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ ifelse(is.na(.x), "Ignorado", .x)),
      dplyr::across(where(is.factor), as.character)
    )

  report$data$render <- df

  rds_file <- tempfile(fileext = ".rds")
  saveRDS(report, rds_file)

  formats <- if (format == "all") c("html", "pdf", "docx") else format

  quarto_format_map <- c(
    html = "html",
    pdf  = "typst",
    docx = "docx"
  )

  ext_map <- c(
    html = "html",
    pdf  = "pdf",
    docx = "docx"
  )

  logo_block <- if (!is.null(logo_path) && file.exists(logo_path)) {
    paste0("![](", normalizePath(logo_path, winslash = "/"), "){width=200px}\n\n")
  } else {
    ""
  }

  # =========================
  # BODY
  # =========================

  body <- paste0(
    "
```{r}
#| include: false

library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

.render_table <- function(name, obj, text = NULL) {

  if (is.null(obj)) return(NULL)

  if (name == \"t1\") return(.render_table1(obj, text))
  if (name %in% c(\"t2\",\"t3\",\"t4\")) return(.render_simple(obj, text, name))

  knitr::kable(obj)
}

.render_table1 <- function(df, text = NULL) {

  df$Variavel <- ifelse(duplicated(df$Variavel), \"\", df$Variavel)

  tab <- knitr::kable(
    df,
    align = paste0(\"ll\", paste(rep(\"c\", ncol(df)-2), collapse = \"\")),
    caption = \"Table 1\"
  )

  tab <- kableExtra::kable_styling(tab, full_width = FALSE)

  if (!is.null(text)) cat(\"\\n\\n\", text, \"\\n\\n\")

  tab
}

.render_simple <- function(df, text = NULL, name = NULL) {

  tab <- knitr::kable(
    df,
    align = paste(rep(\"c\", ncol(df)), collapse = \"\")
  )

  tab <- kableExtra::kable_styling(tab, full_width = FALSE)

  if (!is.null(text)) cat(\"\\n\\n\", text, \"\\n\\n\")

  tab
}

report <- readRDS(params$report_path)
```

", logo_block, "

# Relatório dataLGBT

## Tabelas

```{r}
#| results: asis

for (nm in names(report$tables)) {

  obj  <- report$tables[[nm]]
  text <- report$text[[nm]]

  if (is.null(obj)) next

  cat(\"\\n\\n### \", toupper(nm), \"\\n\\n\", sep = \"\")

  .render_table(nm, obj, text)
}
```

## Gráficos

```{r}
#| results: asis

for (nm in names(report$graphs)) {

  obj <- report$graphs[[nm]]

  if (is.null(obj)) next

  cat(\"\\n\\n### \", nm, \"\\n\\n\", sep = \"\")

  print(obj)
}
```
"
  )

  # =========================
  # RENDER (COMPATÍVEL)
  # =========================

  results <- list()
  outputs <- list()

  old_wd <- getwd()
  setwd(out_dir)
  on.exit(setwd(old_wd), add = TRUE)

  for (fmt in formats) {

    qmd_file <- tempfile(fileext = ".qmd")

    yaml <- paste0(
      "---
title: \"Relatório dataLGBT\"
author: \"", autores, "\"
format:
  ", quarto_format_map[[fmt]], ":
    toc: true
params:
  report_path: NULL
---
"
    )

    writeLines(c(yaml, body), qmd_file)

    # Nome do arquivo esperado
    out_file <- paste0(output_file, ".", ext_map[[fmt]])

    out <- tryCatch({

      # Renderiza (SEM output_dir)
      quarto::quarto_render(
        input = qmd_file,
        output_format = quarto_format_map[[fmt]],
        output_file = out_file,
        execute_params = list(report_path = rds_file),
        quiet = TRUE
      )

      # 🔎 Procurar onde o Quarto salvou (WD ou tempdir)
      possible_paths <- c(
        file.path(out_dir, out_file),
        file.path(tempdir(), out_file)
      )

      generated_file <- possible_paths[file.exists(possible_paths)][1]

      if (is.na(generated_file)) {

        # fallback mais agressivo: busca no tempdir
        candidates <- list.files(
          tempdir(),
          pattern = out_file,
          full.names = TRUE,
          recursive = TRUE
        )

        if (length(candidates) == 0) {
          stop("Arquivo não encontrado após renderização")
        }

        generated_file <- candidates[1]
      }

      # Caminho final
      final_file <- file.path(out_dir, out_file)

      # Copia (se necessário)
      if (!normalizePath(generated_file) == normalizePath(final_file)) {
        file.copy(generated_file, final_file, overwrite = TRUE)
      }

      outputs[[fmt]] <- final_file

      paste0("OK: ", final_file)

    }, error = function(e) {

      paste("FAIL:", e$message)
    })

    results[[fmt]] <- out
  }

  # =========================
  # VALIDAÇÃO
  # =========================

  failed <- grepl("^FAIL:", unlist(results))

  if (all(failed)) {
    stop(
      "Falha em todos os formatos de saída:\n",
      paste(unlist(results), collapse = "\n")
    )
  }

  if (any(failed)) {
    warning(
      "Alguns formatos falharam:\n",
      paste(unlist(results)[failed], collapse = "\n")
    )
  }

  return(list(
    status = results,
    output = outputs
  ))

}
