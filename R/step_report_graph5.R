# =========================
# STEP: REPORT GRAPH 5 (CAUSAS POR SGM)
# =========================

step_report_graph5 <- function(report) {

  if (!inherits(report, "dataLGBT_report")) {
    stop("`report` deve ser um objeto da classe 'dataLGBT_report'.")
  }

  top_n <- report$meta$graph5_top_n %||% 8

  df <- report$data$raw

    idioma      <- report$meta$idioma %||% "pt"
    sogi.filter <- report$meta$sogi.filter %||% "all"

    # -------------------------------------------------------
    # 1. Checagens
    # -------------------------------------------------------
    required_vars <- c("Grupo_CID", "SGM", "Morte")

    if (!all(required_vars %in% names(df))) {

      warning("Variáveis necessárias para graph5 não encontradas.")

      report$graphs$g5 <- NULL
      report$text$g5 <- NULL

      return(report)
    }

    if (!(sogi.filter %in% c("all", "lgbt"))) {

      report$graphs$g5 <- NULL
      report$text$g5 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 2. Filtrar óbitos
    # -------------------------------------------------------
    df <- df %>%
      dplyr::filter(Morte == "1 - Sim")

    if (nrow(df) == 0) {

      report$graphs$g5 <- NULL
      report$text$g5 <- NULL

      return(report)
    }

    # -------------------------------------------------------
    # 3. Limpeza
    # -------------------------------------------------------
    df$Grupo_CID <- as.character(df$Grupo_CID)
    df$SGM <- as.character(df$SGM)

    df$Grupo_CID[is.na(df$Grupo_CID)] <- "Ignorado"
    df$SGM[is.na(df$SGM)] <- "Ignorado"

    # -------------------------------------------------------
    # 4. Selecionar TOP causas (global)
    # -------------------------------------------------------
    causas_top <- df %>%
      dplyr::count(Grupo_CID, sort = TRUE) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(Grupo_CID)

    df$Grupo_CID_plot <- ifelse(
      df$Grupo_CID %in% causas_top,
      df$Grupo_CID,
      ifelse(idioma == "pt", "Outras causas", "Other causes")
    )

    # -------------------------------------------------------
    # 5. Agregar
    # -------------------------------------------------------
    df_plot <- df %>%
      dplyr::group_by(SGM, Grupo_CID_plot) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(SGM) %>%
      dplyr::mutate(prop = n / sum(n)) %>%
      dplyr::ungroup()

    # -------------------------------------------------------
    # 6. Ordenação das causas (global)
    # -------------------------------------------------------
    ordem_causas <- df %>%
      dplyr::count(Grupo_CID_plot, sort = TRUE) %>%
      dplyr::pull(Grupo_CID_plot)

    df_plot$Grupo_CID_plot <- factor(
      df_plot$Grupo_CID_plot,
      levels = ordem_causas
    )

    # -------------------------------------------------------
    # 7. Gráfico (100% stacked)
    # -------------------------------------------------------
    g5 <- ggplot2::ggplot(
      df_plot,
      ggplot2::aes(x = SGM, y = prop, fill = Grupo_CID_plot)
    ) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1)
      ) +
      ggplot2::labs(
        title = ifelse(idioma == "pt",
                       "Distribuição das Causas de Morte por SGM",
                       "Distribution of Causes of Death by SGM"),
        x = "SGM",
        y = ifelse(idioma == "pt", "Proporção (%)", "Proportion (%)"),
        fill = ifelse(idioma == "pt", "Grupo CID", "ICD Group")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold")
      )

    # -------------------------------------------------------
    # 8. Texto interpretativo
    # -------------------------------------------------------
    top_global <- ordem_causas[1]

    if (idioma == "pt") {

      texto5 <- paste0(
        "O Gráfico 5 apresenta a distribuição das causas de morte segundo SGM. ",
        "Observa-se que o grupo de causas mais frequente no conjunto dos dados foi ",
        top_global, ", com variações na composição entre os grupos."
      )

    } else {

      texto5 <- paste0(
        "Graph 5 shows the distribution of causes of death by SGM. ",
        "The most frequent cause group overall was ", top_global,
        ", with variation across groups."
      )
    }

    # -------------------------------------------------------
    # 9. Salvar
    # -------------------------------------------------------
    report$graphs$g5 <- g5
    report$text$g5 <- texto5

    report$steps <- c(report$steps, "graph5")

    report$log$graph5 <- list(
      timestamp = Sys.time(),
      n_sgm = length(unique(df_plot$SGM)),
      n_causas = length(unique(df_plot$Grupo_CID_plot))
    )

    return(report)
  }
