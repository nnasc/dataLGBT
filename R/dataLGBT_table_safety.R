# =========================
# TABLE SAFETY LAYER
# =========================

.dataLGBT_table_safety <- function(x,
                                   na_char = "0 (0%)",
                                   na_num = 0,
                                   empty_char = "Ignorado") {

  if (!is.data.frame(x)) {
    stop("Table safety layer requires a data.frame.")
  }

  x <- as.data.frame(x)

  # -------------------------
  # 1. Character columns
  # -------------------------
  x <- dplyr::mutate(
    x,
    dplyr::across(
      where(is.character),
      ~ dplyr::if_else(
        is.na(.x) | trimws(.x) == "",
        empty_char,
        .x
      )
    )
  )

  # -------------------------
  # 2. Factor columns
  # -------------------------
  x <- dplyr::mutate(
    x,
    dplyr::across(
      where(is.factor),
      ~ dplyr::if_else(
        is.na(as.character(.x)) | trimws(as.character(.x)) == "",
        empty_char,
        as.character(.x)
      )
    )
  )

  # -------------------------
  # 3. Numeric columns
  # -------------------------
  x <- dplyr::mutate(
    x,
    dplyr::across(
      where(is.numeric),
      ~ ifelse(is.na(.x), na_num, .x)
    )
  )

  # -------------------------
  # 4. Final safety sweep
  # -------------------------
  x[is.na(x)] <- na_char

  return(x)
}
