
#' Creates a text summary
#' @param txt An object that can be converted to a text.
#' @param ... Currently unused.
#' @inheritParams summary_scalar
#' @family summary
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   log_event(
#'     x = "hello world",
#'     y = summary_text("hello world")
#'   )
#' })
#' @returns A summary that can be logged with [log_event()].
#' @export
summary_text <- function(txt, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_text")
}

#' @describeIn summary_text Creates a summary from a scalar character vector.
#' @export
summary_text.character <- function(txt, ..., metadata = NULL, tag = NA) {
  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "text")
  }

  if (!rlang::is_scalar_atomic(txt)) {
    cli::cli_abort(c(
      "Can't log a character vector with length != 1.",
      i = "Expected a single value but got a vector with length {.val {length(txt)}}."
    ))
  }

  if (!all(field(metadata, "plugin_name") == "text")) {
    cli::cli_abort(c(
      "Plugin name should be 'text'",
      x = "Got {.val {unique(field(metadata, 'plugin_name'))}}"
    ))
  }

  summary_tensor(
    txt,
    dtype = "string",
    metadata = metadata,
    tag = tag
  )
}
