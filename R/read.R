collect_events <- function(logdir = get_default_logdir()) {
  rlang::check_installed("tibble")
  files <- fs::dir_ls(logdir, type = "file", regexp = ".*tfevents.*",
                      recurse = TRUE)
  events <- lapply(files, event_file_iterator_collect)
  events <- lapply(events, do.call, what = vec_c)

  tb <- map2(events, function(ev, path) {
    tibble::tibble(
      run = fs::path_dir(fs::path_rel(path, logdir)),
      event = ev
    )
  })

  events <- vec_rbind(!!!tb)
  events$type <- field(events$event, "type")
  events$name <- field(events$event, "name")
  events$step <- field(events$event, "step")
  events$summary <- field(events$event, "summary")
  events
}

collect_summaries <- function(logdir = get_default_logdir()) {
  events <- collect_events(logdir)
  events[events$type == "summary", ]
}

collect_scalars <- function(logdir = get_default_logdir()) {
  summaries <- collect_summaries(logdir)
  summaries$value <- field(summaries$summary, "value")
  summaries[!is.na(summaries$value),]
  summaries
}
