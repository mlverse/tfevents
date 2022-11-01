create_event_writer <- function(logdir) {
  fs::dir_create(logdir, recurse = TRUE)
  fname <- file.path(
    logdir,
    paste("events.out.tfevents.", as.integer(Sys.time()), ".v2")
  )
  event_writer(fname)
}
