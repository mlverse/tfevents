#' @export
summary_text <- function(txt, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_text")
}

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
