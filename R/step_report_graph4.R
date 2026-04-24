# =========================
# STEP: REPORT GRAPH 4 (CAUSAS DE MORTE)
# =========================

step_report_graph4 <- function(top_n = 10) {

  function(report) {

    if (!inherits(report, "dataLGBT_report")) {
      stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
    }

    df <- report$data$raw

    idioma <- report$meta$idioma %||% "pt"

    # -------------------------------------------------------
    # 1. Checagens
    # -------------------------------------------------------
    required_vars <- c("Grupo_CID", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para graph4 não encontradas.")

      report$graphs$g4 <- NULL
      report$text$g4 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Filtrar óbitos
    # -------------------------------------------------------
    df <- df %>%
      dplyr::filter(Morte == "1 - Sim")

    if (nrow(df) == 0) {

      report$graphs$g4 <- NULL
      report$text$g4 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 3. Limpar variável
    # -------------------------------------------------------
    df$Grupo_CID <- as.character(df$Grupo_CID)
    df$Grupo_CID[is.na(df$Grupo_CID) | trimws(df$Grupo_CID) == ""] <- "Ignorado"

    # -------------------------------------------------------
    # 4. Agregar
    # -------------------------------------------------------
    df_plot <- df %>%
      dplyr::count(Grupo_CID, name = "n") %>%
      dplyr::mutate(prop = n / sum(n)) %>%
      dplyr::arrange(dplyr::desc(n))

    # -------------------------------------------------------
    # 5. Top causas + "Outras"
    # -------------------------------------------------------
    if (nrow(df_plot) > top_n) {

      top <- df_plot[1:top_n, ]
      outras <- df_plot[(top_n + 1):nrow(df_plot), ]

      outras_row <- data.frame(
        Grupo_CID = ifelse(idioma == "pt", "Outras causas", "Other causes"),
        n = sum(outras$n),
        prop = sum(outras$prop)
      )

      df_plot <- dplyr::bind_rows(top, outras_row)
    }

    # -------------------------------------------------------
    # 6. Ordenação (IMPORTANTE)
    # -------------------------------------------------------
    df_plot$Grupo_CID <- factor(
      df_plot$Grupo_CID,
      levels = rev(df_plot$Grupo_CID)
    )

    # -------------------------------------------------------
    # 7. Labels epidemiológicos
    # -------------------------------------------------------
    df_plot$label <- paste0(
      sprintf("%.1f", df_plot$prop * 100), "%"
    )

    # -------------------------------------------------------
    # 8. Gráfico (horizontal = padrão paper)
    # -------------------------------------------------------
    g4 <- ggplot2::ggplot(
      df_plot,
      ggplot2::aes(x = Grupo_CID, y = prop)
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        hjust = -0.1,
        size = 3
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(
        title = ifelse(idioma == "pt",
                       "Distribuição das Causas de Morte (CID-10)",
                       "Distribution of Causes of Death (ICD-10)"),
        x = NULL,
        y = ifelse(idioma == "pt", "Proporção (%)", "Proportion (%)")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(face = "bold")
      )

    # -------------------------------------------------------
    # 9. Texto interpretativo
    # -------------------------------------------------------
    principal <- df_plot$Grupo_CID[which.max(df_plot$prop)]

    if (idioma == "pt") {

      texto4 <- paste0(
        "O Gráfico 4 apresenta a distribuição das causas de morte segundo grupos do CID-10. ",
        "O grupo mais frequente foi ", principal, ", representando ",
        sprintf("%.1f", max(df_plot$prop) * 100), "% dos óbitos."
      )

    } else {

      texto4 <- paste0(
        "Graph 4 shows the distribution of causes of death by ICD-10 groups. ",
        "The most frequent group was ", principal, ", accounting for ",
        sprintf("%.1f", max(df_plot$prop) * 100), "% of deaths."
      )
    }

    # -------------------------------------------------------
    # 10. Salvar
    # -------------------------------------------------------
    report$graphs$g4 <- g4
    report$text$g4 <- texto4

    report$steps <- c(report$steps, "graph4")

    report$log$graph4 <- list(
      timestamp = Sys.time(),
      n_causas = nrow(df_plot),
      top_n = top_n
    )

    return(report)
  }
}
