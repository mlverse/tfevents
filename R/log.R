log_event <- function(..., step = NULL) {
  data <- rlang::dots_list(..., .named = TRUE, .homonyms = "error")
  step <- get_global_step()
  map2(data, write_event, step = step)
  invisible(data)
}

#' Write event data into disk
#'
#' @param data Event data.
#' @param name Name/tag of the event data.
#' @param step Global step number
#'
#' @keywords internal
#'
#' @export
write_event <- function(data, name, step) {
  UseMethod("write_event")
}

#' @export
write_event.default <- function(data, name, step) {
  cli::cli_abort("Cant write event with type {.cls {class(data)}}.")
}

#' @export
write_event.list <- function(data, name, step) {
  with_logdir(file.path(get_default_logdir(), name), {
    map2(data, write_event, step = step)
  })
}

#' @export
write_event.summary_scalar <- function(data, name, step) {
  metadata <- field(data, "metadata")

  write_scalar(
    writer = get_writer(),
    name = name,
    step = step,
    data = field(data, "value"),
    description = field(metadata, "description"),
    display_name = field(metadata, "display_name")
  )
}

#' @export
write_event.numeric <- function(data, name, step) {
  event <- summary_scalar(data)
  write_event(event, name, step)
}

.tfevents <- new.env()

get_default_logdir <- function() {
  if (is.null(.tfevents$logdir))
    set_default_logdir()
  rlang::env_get(.tfevents, "logdir")
}
set_default_logdir <- function(logdir = "logs") {
  rlang::env_bind(.tfevents, logdir = path.expand(logdir))
}
with_logdir <- withr::with_(
  set = set_default_logdir,
  get = function(logdir) {
    get_default_logdir()
  }
)

.steps <- new.env()
get_global_step <- function() {
  # a separate global step count is tracked for each root logdir.
  # the global step is queried once when calling `log_event`.
  logdir <- get_default_logdir()
  if (!rlang::env_has(.steps, logdir))
    set_global_step(-1)
  cur_step <- rlang::env_get(.steps, logdir) + 1L
  set_global_step(cur_step)
  cur_step
}
set_global_step <- function(step) {
  rlang::env_poke(.steps, get_default_logdir(), as.integer(step), create = TRUE)
}

.writers <- new.env()
get_writer <- function(logdir = get_default_logdir()) {
  writer <- rlang::env_get(.writers, nm = logdir, default = NULL)
  if (!is.null(writer))
    return(writer)

  writer <- create_event_writer(logdir)
  rlang::env_poke(
    .writers,
    logdir,
    writer,
    create = TRUE
  )
  writer
}

