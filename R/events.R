
event <- function(type, wall_time, step, ..., name = NA, summary = NA,
                  file_version = NA) {
  new_event(
    type = vec_cast(type, character()),
    wall_time = vec_cast(wall_time, integer()),
    step = vec_cast(step, integer()),
    name = vec_cast(name, character()),
    summary = vec_cast(summary, new_summary()),
    file_version = vec_cast(file_version, character())
  )
}

new_event <- function(type = character(),
                      wall_time = integer(),
                      step = integer(), ...,
                      name = character(),
                      summary = new_summary(),
                      file_version = character()) {
  vctrs::new_rcrd(
    fields = list(
      type = type,
      wall_time = wall_time,
      step = step,
      name = name,
      summary = summary
    ),
    class = "tfevents_event"
  )
}

tfevent <- function(run, wall_time, step, ..., summary = NA, file_version = NA) {
  new_tfevent(
    run = vec_cast(run, character()),
    wall_time = vec_cast(wall_time, integer()),
    step = vec_cast(step, integer()),
    summary = vec_cast(summary, new_summary()),
    file_version = vec_cast(file_version, character())
  )
}

new_tfevent <- function(run = character(),
                        wall_time = integer(),
                        step = integer(),
                        ...,
                        summary = new_summary(),
                        file_version = character()) {
  new_rcrd(list(
    wall_time = wall_time,
    step = step,
    summary = summary,
    file_version = file_version
  ), class = "tfevent")
}


#' @export
format.tfevent <- function(x, ...) {
  paste0("<", field(x, "step"),"/", format(field(x, "summary")), ">")
}

#' @export
as_tfevent <- function(x, step, wall_time, ...) {
  UseMethod("as_tfevent")
}

#' @export
as_tfevent.list <- function(x, step, wall_time, ..., name = ".") {
  ev <- map2(
    x,
    function(obj, nm) {
      as_tfevent(obj, step = step, wall_time = wall_time, name = c(name, nm))
    }
  )
  vec_c(!!!rlang::squash_if(unname(ev), vec_is_list))
}

#' @export
as_tfevent.numeric <- function(x, step, wall_time, ..., name) {
  x <- summary_scalar(x)
  as_tfevent(x, step = step, wall_time = wall_time, name = name)
}

#' @export
as_tfevent.tfevents_summary <- function(x, step, wall_time, ..., name) {
  field(x, "tag") <- tail(name, 1)
  tfevent(
    run = paste0(name[-length(name)], collapse = "/"),
    wall_time = wall_time,
    step = step,
    summary = x
  )
}

#' @export
vec_ptype2.tfevents_event.tfevents_event <- function(x, y, ...) {
  x
}

#' @export
vec_cast.tfevents_event.tfevents_event <- function(x, to, ...) {
  x
}

#' @import vctrs
#' @export
format.tfevents_event <- function(x, ...) {
  paste0(
    "<",
    format(field(x, "type"), trim = TRUE, justify="none"),
    "/",
    format(field(x, "name"), trim = TRUE, justify="none"),
    ">"
  )
}

#' Scalar event
#'
#' @param value A numeric scalar value to be logged.
#' @param ... Currently unused. To allow future expansion.
#' @param metadata A `metadata` object, as created with [summary_metadata()]. In
#'   most cases you don't need to change the default.
#'
#'
#' @returns A `scalar_event` object.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   log_event(loss = summary_scalar(1))
#' })
#' @export
summary_scalar <- function(value, ..., metadata = NULL) {
  ellipsis::check_dots_empty()
  new_summary_scalar(value, metadata = metadata, tag = NA)
}

new_summary_scalar <- function(value = numeric(), ..., metadata = NULL) {
  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "scalars")
  }
  tfevents_summary(metadata = metadata, value = value, class = "summary_scalar")
}

tfevents_summary <- function(metadata, tag = NA, ..., value = NA, image = NA, class = NULL) {
  value <- vec_cast(value, numeric())
  image <- vec_cast(image, new_image_impl())
  new_summary(metadata = metadata, tag = tag, value = value, image = image, class = class)
}

new_summary <- function(metadata = new_summary_metadata(), tag = character(), ...,
                        value = numeric(), image = new_image_impl(),
                        class = NULL) {
  vctrs::new_rcrd(
    fields = list(metadata = metadata, tag = tag, value = value, image = image),
    class = c(class, "tfevents_summary")
  )
}

#' @export
vec_ptype2.tfevents_summary.tfevents_summary <- function(x, y, ...) {
  x
}

#' @export
vec_cast.tfevents_summary.tfevents_summary <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.tfevent.tfevent <- function(x, y, ...) {
  x
}

#' @export
vec_cast.tfevent.tfevent <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.summary_scalar.tfevents_summary <- function(x, y, ...) {
  y
}

#' @export
vec_cast.tfevents_summary.summary_scalar <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.summary_image.tfevents_summary <- vec_ptype2.summary_scalar.tfevents_summary

#' @export
vec_cast.tfevents_summary.summary_image <- vec_cast.tfevents_summary.summary_scalar

#' Summary metadata
#'
#' Creates a summary metadata that can be passed to multiple `summary_` functions.
#'
#' @param plugin_name The name of the TensorBoard plugin that might use the summary.
#' @param display_name Display name for the summary.
#' @param description A description of the summary.
#'
#' @returns A `summary_metadata` object.
#'
#' @examples
#' summary <- summary_scalar(1, metadata = summary_metadata("scalars"))
#'
#' @export
summary_metadata <- function(
    plugin_name,
    display_name = NA_character_,
    description = NA_character_) {
  new_summary_metadata(plugin_name = plugin_name, display_name = display_name,
                       description = description)
}

new_summary_metadata <- function(plugin_name = character(), display_name = character(),
                                 description = character()) {
  vctrs::new_rcrd(
    fields = list(
      plugin_name = plugin_name,
      display_name = display_name,
      description = description
    ),
    class = "tfevents_summary_metadata"
  )
}

#' @export
format.tfevents_summary <- function(x, ...) {
  format(vctrs::field(x, "value"))
}
