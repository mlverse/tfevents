log_event <- function(..., step = NULL) {
  data <- rlang::dots_list(..., .named = TRUE, .homonyms = "error")
  step <- get_global_step()
  map2(data, write_event, step = step)
}

write_event <- function(data, name, step) {
  UseMethod("write_event")
}

write_event.default <- function(data, name, step) {
  cli::cli_abort("Cant write event with type {.cls {class(data)}}.")
}

write_event.list <- function(data, name, step) {
  with_logdir(file.path(get_default_logdir(), name), {
    map2(data, write_event, step = step)
  })
}

write_event.numeric <- function(data, name, step) {
  write_scalar(
    writer = get_writer(),
    data = data,
    name = name,
    step = step,
    description = ""
  )
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

get_global_step <- function() {
  if (is.null(.tfevents$step))
    set_global_step(-1)
  .tfevents$step <- .tfevents$step + 1L
  rlang::env_get(.tfevents, "step")
}
set_global_step <- function(step) {
  rlang::env_bind(.tfevents, step = as.integer(step))
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

