#' Scalar event
#'
#' @param value A numeric scalar value to be logged.
#' @param ... Currently unused. To allow future expansion.
#' @param metadata A `metadata` object, as created with [summary_metadata()]. In
#'   most cases you don't need to change the default.
#' @param tag A tag that within the TensorBoard UI. See [log_event()] for other
#'   ways of specifying the tag attribute.
#'
#' @returns A `<scalar_event>` object.
#' @family summary
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   log_event(loss = summary_scalar(1))
#' })
#' @export
summary_scalar <- function(value, ..., metadata = NULL, tag = NA) {
  rlang::check_dots_empty()
  new_summary_scalar(value, metadata = metadata, tag = tag)
}

new_summary_scalar <- function(value = numeric(), ..., metadata = NULL,
                               tag = character()) {
  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "scalars")
  }
  summary_values(metadata = metadata, value = value, tag = tag,
                 class = "tfevents_summary_scalar")
}

#' @export
vec_ptype2.tfevents_summary_values.tfevents_summary_scalar <- function(x, y, ...) {
  new_summary_values()
}
#' @export
vec_ptype2.tfevents_summary_scalar.tfevents_summary_values <- function(x, y, ...) {
  new_summary_values()
}
