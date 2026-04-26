step_proc_drop_raw <- function() {

  function(pipe) {

    if (!inherits(pipe, "data_link_pipe") ||
        is.null(pipe$data$proc$data)) {
      stop("`proc_core()` deve ser executado antes de `step_proc_drop_raw()`.", call. = FALSE)
    }

    df <- pipe$data$proc$data
    raw_vars <- pipe$proc_meta$raw_vars

    if (is.null(raw_vars)) {
      stop("`raw_vars` não foi definido em `proc_core()`.", call. = FALSE)
    }

    df <- dplyr::select(df, -dplyr::all_of(intersect(raw_vars, names(df))))

    pipe$data$proc$data <- df
    pipe$data$proc$steps <- c(pipe$data$proc$steps, "drop_raw")
    pipe$proc_meta$steps <- c(pipe$proc_meta$steps, "drop_raw")

    return(pipe)
  }
}
