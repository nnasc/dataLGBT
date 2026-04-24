# =====================================
# HELPER: BUILD TIME SERIES
# =====================================

.dataLGBT_build_time_series <- function(df,
                                        var_data,
                                        filtro = NULL,
                                        time.comp = "ano",
                                        idioma = "pt") {

  if (!is.null(filtro)) {
    df <- df[filtro, , drop = FALSE]
  }

  if (!(var_data %in% names(df))) {
    return(NULL)
  }

  df[[var_data]] <- as.Date(df[[var_data]])
  df <- df[!is.na(df[[var_data]]), , drop = FALSE]

  if (nrow(df) == 0) {
    return(NULL)
  }

  # -------------------------
  # Criar eixo temporal
  # -------------------------
  if (time.comp == "ano") {
    df$tempo <- format(df[[var_data]], "%Y")
    x_lab <- ifelse(idioma == "pt", "Ano", "Year")

  } else if (time.comp == "mes") {
    df$tempo <- format(df[[var_data]], "%Y-%m")
    x_lab <- ifelse(idioma == "pt", "Ano-Mês", "Year-Month")

  } else if (time.comp == "dia") {
    df$tempo <- as.character(df[[var_data]])
    x_lab <- ifelse(idioma == "pt", "Data", "Date")

  } else {
    return(NULL)
  }

  # -------------------------
  # Agregação
  # -------------------------
  df_plot <- df %>%
    dplyr::count(tempo, name = "n") %>%
    dplyr::arrange(tempo)

  return(list(
    data = df_plot,
    x_lab = x_lab
  ))
}
