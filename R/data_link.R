#' Run probabilistic linkage between SINAN and SIM
#'
#' @param df_sinan Data.frame or list of data.frames (SINAN)
#' @param df_sim Data.frame or list of data.frames (SIM)
#' @param dedup_sim Logical. Whether to deduplicate SIM
#' @param filter_sogi Filter for SOGI groups
#' @param threshold Matching threshold (default = 0.35)
#' @param save_summary Logical. Save linkage summary
#' @param summary_path File path for summary
#'
#' @return A data_link_pipe object
#' @export
data_link <- function(df_sinan,
                      df_sim,
                      dedup_sim = FALSE,
                      filter_sogi = c("all","lgbt","gay","lesbian",
                                      "bissexual","travesti",
                                      "mul.trans","hom.trans"),
                      threshold = 0.35,
                      save_summary = TRUE,
                      summary_path = "linkage_summary.txt") {
  
  # -------------------------
  # 1. Validar argumentos
  # -------------------------
  filter_sogi <- match.arg(filter_sogi)
  
  # -------------------------
  # 2. Validar bases
  # -------------------------
  sinan_val <- data_link_validate(df_sinan, role = "sinan")
  sim_val   <- data_link_validate(df_sim, role = "sim")
  
  # -------------------------
  # 3. Inicializar pipeline
  # -------------------------
  pipe <- data_link_pipe(sinan_val, sim_val)
  
  # -------------------------
  # 4. Pipeline
  # -------------------------
  
  pipe <- pipe_add_step(pipe, "filter_sogi", step_filter_sogi(filter_sogi))
  
  pipe <- pipe_add_step(pipe, "standardize", step_standardize())
  
  pipe <- pipe_add_step(pipe, "clean", step_clean())
  
  pipe <- pipe_add_step(pipe, "dedup", step_dedup(dedup_sim = dedup_sim))
  
  pipe <- pipe_add_step(pipe, "linkage", step_linkage(threshold = threshold))
  
  pipe <- pipe_add_step(
    pipe,
    "link_summary",
    step_link_summary(
      save_summary = save_summary,
      summary_path = summary_path
    )
  )
  
  return(pipe)
}