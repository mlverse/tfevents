
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
#' @export
summary_scalar <- function(value, ...) {
  new_summary_scalar(value, ...)
}

new_summary_scalar <- function(value = numeric(), ...) {
  metadata <- summary_metadata(plugin_name = "scalars", ...)
  new_summary(metadata = metadata, value = value)
}

tfevents_summary <- function(metadata, ..., value = NA) {
  new_summary(metadata = metadata, value = value)
}

new_summary <- function(metadata = new_summary_metadata(), ..., value = numeric()) {
  vctrs::new_rcrd(
    fields = list(metadata = metadata, value = value),
    class = c("tfevents_summary")
  )
}

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
