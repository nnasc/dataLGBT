# =========================
# PACKAGE UTILITIES
# =========================

# -------------------------
# Import pipe operator
# -------------------------

#' @importFrom magrittr %>%
NULL


# -------------------------
# Import utils functions
# -------------------------

#' @importFrom utils capture.output
NULL


# -------------------------
# Global variables (tidy eval)
# -------------------------

utils::globalVariables(c(
  ".name_std",
  ".mother_name_std",
  ".birth_date_std",
  ".block",
  "id1",
  "id2",
  "posterior",
  "sogi_group",
  "NM_PACIENT",
  "NM_MAE_PAC",
  "DT_NASC",
  "NOME",
  "NOMEMAE",
  "DTNASC"
))
