create_event_writer <- function(logdir) {
  fs::dir_create(logdir, recurse = TRUE)
  fname <- file.path(
    logdir,
    paste0("events.out.tfevents.", as.integer(Sys.time()), ".v2")
  )
  fs::file_create(fname)
  event_writer(fname)
}
