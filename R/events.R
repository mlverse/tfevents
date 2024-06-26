#' Coerce an object to a event.
#'
#' @param x Object that will be coerced to an event.
#' @param step The step that will be used when the event is logged. This is used
#'   by TensorBoard when showing data.
#' @param wall_time The all time the event will appended to the event. This field
#'   is used by TensorBoard when displaying information based on actual time.
#' @param ... currently unused.
#'
#' @returns A event vctr with class <tfevents_event>.
#'
#' @section Extending `as_event`:
#'
#' `as_event` is an S3 generic and you can implement method for your own class.
#' We don't export the `event` constructor though, so you should implement it
#' in terms of other `as_event` methods.
#'
#' @examples
#' as_event(list(hello = 1), step = 1, wall_time = 1)
#'
#' @export
as_event <- function(x, step, wall_time, ...) {
  UseMethod("as_event")
}

#' @export
as_event.list <- function(x, step, wall_time, ..., name = ".") {
  ev <- map2(
    x,
    function(obj, nm) {
      as_event(obj, step = step, wall_time = wall_time, name = c(name, nm))
    }
  )
  vec_c(!!!squash_if(unname(ev), vec_is_list))
}

squash_if <- function(x, predicate) {
  lapply(x, function(obj) {
    if (predicate(obj)) {
      unlist(obj)
    } else {
      obj
    }
  })
}

#' @export
as_event.numeric <- function(x, step, wall_time, ..., name) {
  if (!rlang::is_scalar_atomic(x)) {
    cli::cli_abort(c(
      "Can't log a numeric vector with length != 1.",
      i = "Expected a single value but got a vector with length {.val {length(x)}}."
    ))
  }
  x <- summary_scalar(x)
  as_event(x, step = step, wall_time = wall_time, name = name)
}

#' @export
as_event.character <- function(x, step, wall_time, ..., name) {
  x <- summary_text(x)
  as_event(x, step = step, wall_time = wall_time, name = name)
}

#' @importFrom utils tail
#' @export
as_event.tfevents_summary_values <- function(x, step, wall_time, ..., name) {
  field(x, "tag") <- make_tag(field(x, "tag"), tail(name, 1))
  event(
    run = paste0(name[-length(name)], collapse = "/"),
    wall_time = wall_time,
    step = step,
    summary = summary(list(x))
  )
}

make_tag <- function(cur_tag, name) {
  name <- rep(name, length(cur_tag))

  if (any(name[!is.na(cur_tag)] != "")) {
    cli::cli_abort(c(
      x = "Two tags were provided for the same summary.",
      i = "You can only a specify tags once for a summary."
    ))
  }

  cur_tag[is.na(cur_tag)] <- name[is.na(cur_tag)]

  if (any(cur_tag == "")) {
    cli::cli_abort(c(
      x = "All summaries must have a tag, but found at least one without one.",
      i = "See {.help log_event} to find out how to specify tags for summaries."
    ))
  }

  cur_tag
}

#' Creates events
#'
#' We try to match events as closely as possible to the protobuf messages.
#' The hierarchy looks like:
#' ```
#' event (<event>):
#'  - run (<character>)
#'  - wall_time (<integer>)
#'  - step (<integer>)
#'  - summary (<summary> aka list_of<summary_values>):
#'     - values (list):
#'       - <summary_value>:
#'         - metadata (<summary_metadata>)
#'         - tag (<character>)
#'         - value (<numeric>)
#'         - image (<summary_summary_image>)
#'           - buffer (<blob>)
#'           - width (<integer>)
#'           - height (<integer>)
#'           - colorspace (<integer>)
#' ```
#'
#' @keywords internal
#' @import vctrs
event <- function(run, wall_time, step, ..., summary = NA, file_version = NA) {
  new_event(
    run = vec_cast(run, character()),
    wall_time = as.integer(wall_time),
    step = as.integer(step),
    summary = vec_cast(summary, new_summary()),
    file_version = vec_cast(file_version, character())
  )
}

new_event <- function(run = character(),
                        wall_time = integer(),
                        step = integer(),
                        ...,
                        summary = new_summary(),
                        file_version = character()) {
  new_rcrd(list(
    run = run,
    wall_time = wall_time,
    step = step,
    summary = summary,
    file_version = file_version
  ), class = "tfevents_event")
}


#' @export
format.tfevents_event <- function(x, ...) {
  paste0("<", field(x, "run"),"/", format(field(x, "step")), ">")
}

#' @export
vec_ptype2.tfevents_event.tfevents_event <- function(x, y, ...) {
  x
}

#' @export
vec_cast.tfevents_event.tfevents_event <- function(x, to, ...) {
  x
}

summary_values <- function(metadata, tag = NA, ..., value = NA, image = NA, tensor = NA, class = NULL) {
  value <- vec_cast(value, numeric())
  image <- vec_cast(image, new_summary_summary_image())
  tag <- vec_cast(tag, character())
  tensor <- vec_cast(tensor, new_tensor_proto())

  c(metadata, tag, value, image, tensor) %<-% vec_recycle_common(metadata, tag, value, image, tensor)

  new_summary_values(metadata = metadata, tag = tag, value = value, image = image,
                     tensor = tensor, class = class)
}

new_summary_values <- function(metadata = new_summary_metadata(), tag = character(), ...,
                        value = numeric(), image = new_summary_summary_image(),
                        tensor = new_tensor_proto(), class = NULL) {
  vctrs::new_rcrd(
    fields = list(metadata = metadata, tag = tag, value = value, image = image,
                  tensor = tensor),
    class = c(class, "tfevents_summary_values")
  )
}

summary <- function(values) {
  new_summary(values)
}

new_summary <- function(values = list(new_summary_values())) {
  new_list_of(values, ptype=new_summary_values(), class = "tfevents_summary")
}

#' @export
vec_ptype2.tfevents_summary_values.tfevents_summary_values <- function(x, y, ...) {
  new_summary_values()
}

#' @export
vec_ptype2.tfevents_summary_values.tfevents_summary <- function(x, y, ...) {
  new_summary()
}
#' @export
vec_ptype2.tfevents_summary.tfevents_summary_values <- function(x, y, ...) {
  new_summary()
}

# vec_cast.vctrs_percent.double <- function(x, to, ...) percent(x)
# vec_cast.double.vctrs_percent <- function(x, to, ...) vec_data(x)

#' @export
vec_cast.tfevents_summary_values.tfevents_summary_values <- function(x, to, ...) {
  x
}
#' @export
vec_cast.tfevents_summary.tfevents_summary_values <- function(x, to, ...) {
  if (is.na(x)) return(vec_cast(NA, new_summary()))
  new_summary(list(x))
}


#' Summary metadata
#'
#' Creates a summary metadata that can be passed to multiple `summary_` functions.
#'
#' @param plugin_name The name of the TensorBoard plugin that might use the summary.
#' @param display_name Display name for the summary.
#' @param description A description of the summary.
#' @param plugin_content An optional plugin content. Note that it will only be
#'  used if the C++ function `make_plugin_data` is aware of `plugin_content`
#'  for the specified plugin name. For advanced use only.
#' @param ... Currently unused. For future expansion.
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
    description = NA_character_, ...,
    plugin_content = NA) {
  rlang::check_dots_empty()
  plugin_content <- vec_cast(plugin_content, list())
  new_summary_metadata(plugin_name = plugin_name, display_name = display_name,
                       description = description,
                       plugin_content = plugin_content)
}

new_summary_metadata <- function(plugin_name = character(), display_name = character(),
                                 description = character(), plugin_content = list()) {
  vctrs::new_rcrd(
    fields = list(
      plugin_name = plugin_name,
      display_name = display_name,
      description = description,
      plugin_content = plugin_content
    ),
    class = "tfevents_summary_metadata"
  )
}

#' @export
format.tfevents_summary <- function(x, ...) {
  sapply(x, format)
}

#' @export
format.tfevents_summary_values <- function(x, ...) {
  paste0("<", field(x, "tag"), ">")
}

# These values are used from the C++ code
vec_c_list <- function(x) {
  vec_c(!!!x)
}
na <- NA
is_na <- function(x) {
  if (inherits(x, "vctrs_vctr"))
    vec_any_missing(x)
  else if (is.null(x))
    TRUE
  else
    rlang::is_na(x)
}
