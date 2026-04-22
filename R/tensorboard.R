#' Launch TensorBoard
#'
#' Starts a TensorBoard server to visualize logged events.
#'
#' @param log_dir Path to the log directory.
#' @param host Host to bind TensorBoard to. Defaults to `"127.0.0.1"`.
#' @param port Port to bind TensorBoard to. Defaults to `6060`.
#'
#' @returns A [processx::process] object (invisibly).
#' @export
tensorboard <- function(log_dir, host = "127.0.0.1", port = 6060) {
  rlang::check_installed("reticulate")
  rlang::check_installed("processx")

  reticulate::py_require("tensorboard")
  tb <- file.path(dirname(reticulate::py_config()$python), "tensorboard")

  if (!file.exists(tb)) {
    cli::cli_abort("Unable to find the {.code tensorboard} binary at {.path {tb}}.")
  }

  p <- processx::process$new(
    tb,
    c("--logdir", log_dir, "--host", host, "--port", as.character(port)),
    stdout = "|", stderr = "|"
  )

  Sys.sleep(3)
  if (!p$is_alive()) {
    cli::cli_abort("Failed to launch TensorBoard.")
  }

  url <- paste0("http://", host, ":", port)
  cli::cli_inform("TensorBoard started at {.url {url}}")

  invisible(p)
}
