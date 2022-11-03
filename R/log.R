#' Log event
#'
#' @param ... Named values that you want to log. They can be possibly nested,
#'  in this case, the enclosing names are considered 'run' names by TensorBoard.
#' @param step The step associated the logs. If `NULL`, a managed step counter
#'  will be used, and the global step is increased in every call to [log_event()].
#'
#' @note [log_event()] writes events to the default `logdir`. You can query the
#'  default `logdir` with [get_default_logdir()] and modify it with
#'  [set_default_logdir()]. You can also use the [with_logdir()] context switcher
#'  to temporarily modify the logdir.
#'
#' @return Invisibly returns the logged data.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   log_event(
#'      train = list(loss = runif(1), acc = runif(1)),
#'      valid = list(loss = runif(1), acc = runif(1))
#'   )
#' })
#' @export
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

#' Query and modify the logdir
#'
#' [log_event()] has a notion of default logdir, so you don't need to specify it
#' at every call. These functions allow you to query and the current logdir.
#'
#' @param logdir The `logdir` that you want to set as default.
#' @param new The `logdir` that you want to set as default, used in [with_logdir()].
#' @param code Expressions that will be evaluated in a context with the `new`
#'   `logdir` as the default `logdir`.
#'
#' @returns The `logdir` for `get_default_logdir()` otherwise invisibly returns
#'   `NULL`
#'
#' @examples
#' temp <- tempfile()
#' get_default_logdir()
#' with_logdir(temp, {
#'  print(get_default_logdir())
#' })
#' @export
get_default_logdir <- function() {
  if (is.null(.tfevents$logdir))
    set_default_logdir()
  rlang::env_get(.tfevents, "logdir")
}
#' @describeIn get_default_logdir Modifies the default `logdir`.
#' @export
set_default_logdir <- function(logdir = "logs") {
  rlang::env_bind(.tfevents, logdir = path.expand(logdir))
}
#' @describeIn get_default_logdir Temporarily modify the default `logdir`.
#' @export
with_logdir <- withr::with_(
  set = set_default_logdir,
  get = function(logdir) {
    get_default_logdir()
  }
)

#' Global step counters
#'
#' @param step New value for `step`.
#'
#' @details `tfevents` tracks and automatically increased the step counter whenever
#' [log_event()] is called. Note that, it maintains a separate step counter for
#' each root `logdir`, thus if you change the `logdir` using [set_default_logdir()]
#' or [with_logdir()], a different step counter will be used.
#'
#' @returns The global step value for the default logdir, when `get_global_step`,
#'  otherwise returns `NULL` invisibly.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   print(get_global_step())
#'   set_default_step(100)
#'   print(get_global_step())
#' })
#' print(get_global_step())
#' @export
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
#' @describeIn get_global_step Set the global step.
#' @export
set_global_step <- function(step) {
  rlang::env_poke(.steps, get_default_logdir(), as.integer(step), create = TRUE)
}
.steps <- new.env()

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

