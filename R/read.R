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
  events
}

collect_scalars <- function(logdir = get_default_logdir()) {
  summaries <- collect_summaries(logdir)
  plugin_name <- field(field(summaries$summary, "metadata"), "plugin_name")
  summaries <- summaries[plugin_name=="scalars",]
  summaries$value <- field(summaries$summary, "value")
  summaries
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
