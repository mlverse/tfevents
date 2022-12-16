collect_events <- function(logdir = get_default_logdir()) {
  rlang::check_installed(c("tibble", "tidyr"))
  files <- fs::dir_ls(logdir, type = "file", regexp = ".*tfevents.*", recurse = TRUE)
  events <- lapply(files, event_file_iterator_collect)
  events <- map2(events, function(event, name) {
    field(event, "run") <- rep(fs::path_dir(fs::path_rel(name, logdir)), vec_size(event))
    event
  })
  events <- vec_c(!!!unname(events))
  events <- tibble::tibble(event = events)
  events$run <- field(events$event, "run")
  events$step <- field(events$event, "step")
  events$summary <- field(events$event, "summary")
  events
}

collect_summaries <- function(logdir = get_default_logdir()) {
  events <- collect_events(logdir)
  events <- events[!is.na(events$summary),]
  events$summary <- as.list(events$summary)
  events <- tidyr::unnest(events, summary)
  events$tag <- field(events$summary, "tag")
  events$plugin <- plugin(events$summary)
  events
}

collect_scalars <- function(logdir = get_default_logdir()) {
  summaries <- collect_summaries(logdir)
  summaries <- summaries[summaries$plugin=="scalars",]
  summaries$value <- field(summaries$summary, "value")
  summaries
}

events_iterator <- function(logdir = get_default_logdir()) {
  force(logdir)
  files <- fs::dir_ls(logdir, type = "file", regexp = ".*tfevents.*", recurse = TRUE)
  iterators <- create_iterators(files, logdir)
  function() {
    out <- try_iterators(iterators)
    # if there's a value we can ealry return it.
    if (!is_exhausted(out)) {
      return(out)
    }
    # if exhausted, maybe there's a new file in the directory that we were not
    # tracking yet, so we try it before returing the exhausted flag.
    # check if new files were added to the directory
    new_files <- fs::dir_ls(logdir, type = "file", regexp = ".*tfevents.*", recurse = TRUE)
    new_files <- new_files[!new_files %in% files]

    # append to files and iterators
    files <<- c(files, new_files)
    iterators <<- append(iterators, create_iterators(new_files, logdir))

    # retry sendinga value, otherwise returns exhausted().
    try_iterators(iterators)
  }
}

try_iterators <- function(iterators) {
  for (iterator in iterators) {
    event <- try(event_file_iterator_next(iterator), silent = TRUE)
    if (!inherits(event, "try-error")) {
      return(event)
    }
  }
  exhausted()
}

#' @keywords internal
#' @export
value <- function(x, ...) {
  UseMethod("value")
}

#' @export
value.tfevents_summary_values <- function(x, ...) {
  if (!vec_size(x) == 1) {
    cli::cli_abort(c(
      "You must pass a single summary_value to get it's value.",
      i = "Got size {.val {vec_size(x)}}"
    ))
  }
  class(x) <- c(paste0("tfevents_summary_values_", plugin(x)), class(x))
  value(x)
}

#' @export
value.tfevents_summary_values_scalars <- function(x, ...) {
  field(x, "value")
}

#' @export
value.tfevents_summary_values_histograms <- function(x, ...) {
  tensor <- field(x, "tensor")

  values <- field(tensor, "content")[[1]]
  dims <- field(field(tensor, "shape"), "dim")[[1]]

  # values in the tensor arre in C ordering, so we need to move to fortran ordering.
  arr <- aperm(array(values, dim = rev(dims)), perm = rev(seq_along(dims)))

  df <- as.data.frame(arr)
  colnames(df) <- c("lower", "upper", "count")

  df
}

plugin <- function(summary) {
  if (!inherits(summary, "tfevents_summary_values")) {
    cli::cli_abort(c(
      "{.arg summary} must be {.cls tfevents_summary_values}",
      i = "Got object with {.cls {class(summary)}}."
    ))
  }

  field(field(summary, "metadata"), "plugin_name")
}

exhausted <- function () {
  as.symbol(".__exhausted__.")
}

is_exhausted <- function (x) {
  identical(x, exhausted())
}

fill_run_field <- function(event, run) {
  field(event, "run") <- rep(run, vec_size(event))
  event
}

create_iterators <- function(files, logdir) {
  lapply(files, function(name) {
    create_event_file_iterator(
      name,
      fs::path_dir(fs::path_rel(name, logdir))
    )
  })
}
