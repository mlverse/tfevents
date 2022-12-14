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
  iterators <- lapply(files, create_event_file_iterator)
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
    iterators <<- append(iterators, lapply(new_files, create_event_file_iterator))

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
