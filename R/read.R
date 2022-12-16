#' Collect data from tfevents records
#'
#' Collects all events of a kind in a single data.frame ready for analysis.
#'
#' @param logdir The log directory that you want to query events from.
#'
#' @returns
#' A `tibble` with the collected events.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   for(i in 1:5) {
#'     log_event(my_log = runif(1))
#'   }
#' })
#' # collect all events in files, including file description events
#' collect_events(temp)
#' # collect summaries in the logdir
#' collect_summaries(temp)
#' # collect only scalar events
#' collect_scalars(temp)
#'
#' @seealso iter_events
#'
#' @export
collect_events <- function(logdir = get_default_logdir()) {
  iter <- iter_events(logdir)
  vec_rbind(!!!coro::collect(iter))
}

#' @describeIn collect_events Collect events that contain summary values.
#' @export
collect_summaries <- function(logdir = get_default_logdir()) {
  iter <- iter_summaries(logdir)
  vec_rbind(!!!coro::collect(iter))
}

#' @describeIn collect_events Collect event summaries that contain scalar values.
#' @export
collect_scalars <- function(logdir = get_default_logdir()) {
  iter <- iter_scalars(logdir)
  vec_rbind(!!!coro::collect(iter))
}

#' Creates an iterator for events in tfevents records
#'
#' Allows iterating trough events in tfevents records files without necessarily
#' loading all of them in RAM at once. Uses [coro::iterator] protocol.
#'
#' @inheritParams collect_events
#'
#' @returns
#' An iterator that can be used to get events one by one. Returned iterators use
#' the [coro::iterator] protocol.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   for(i in 1:5) {
#'     log_event(my_log = runif(1))
#'   }
#' })
#'
#' # iterate over all events
#' iter <- iter_events(temp)
#' iter()
#' iter()
#' coro::collect(iter)
#'
#' # iterate over summaries only
#' iter <- iter_summaries(logdir)
#' iter()
#' iter()
#' coro::collect(iter)
#'
#' # iterate over events that contain scalar summaries only
#' iter <- iter_scalars(temp)
#' iter()
#' iter()
#' coro::collect(iter)
#'
#' @seealso collect_events
#' @export
iter_events <- function(logdir = get_default_logdir()) {
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

#' @describeIn iter_events Creates an iteartor that discard events that don't contain summaries.
#' @export
iter_summaries <- function(logdir = get_default_logdir()) {
  rlang::check_installed("tidyr")
  iter <- iter_events(logdir)
  coro::gen({
    for (event in iter) {
      if (!is.na(event$summary)) {
        events <- tidyr::unnest(event, summary)
        events$tag <- field(events$summary, "tag")
        events$plugin <- plugin(events$summary)
        coro::yield(events)
      } else {
        next
      }
    }
  })
}

#' @describeIn iter_events Creates an iterator that onyl takes scalar summaries.
#' @export
iter_scalars <- function(logdir = get_default_logdir()) {
  iter <- iter_summaries(logdir)
  coro::gen({
    for (summary in iter) {
      if (summary$plugin == "scalars") {
        summary$value <- value(summary$summary)
        coro::yield(summary)
      } else {
        next
      }
    }
  })
}

try_iterators <- function(iterators) {
  rlang::check_installed("tibble")
  for (iterator in iterators) {
    event <- try(event_file_iterator_next(iterator), silent = TRUE)
    if (!inherits(event, "try-error")) {
      events <- tibble::tibble(event = event)
      events$run <- field(events$event, "run")
      events$step <- field(events$event, "step")
      events$summary <- field(events$event, "summary")
      return(events)
    }
  }
  exhausted()
}

#' Extracts the value of a summary value
#'
#' Summaries are complicated objects because they reflect the Protobuf object
#' structure that are serialized in the tfevents records files. This function
#' allows one to easily query vaues from summaries and will dispatch to the
#' correct way to extract images, audio, text, etc from summary values.
#'
#' @param x A `tfevents_summary_values` object.
#' @param ... Currently unused. To allow future extension.
#' @returns
#' Depending on the type of the summary it returns an image, audio, text or
#' scalar.
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   for(i in 1:5) {
#'     log_event(my_log = runif(1))
#'   }
#' })
#'
#' # iterate over all events
#' iter <- iter_summaries(temp)
#' summary <- iter()
#' value(summary$summary)
#'
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

#' @export
value.tfevents_summary_values_audio <- function(x, ...) {
  # audio is stored in the tensor field.
  tensor <- field(x, "tensor")

  # content is serialized as a wave encoded tensor.
  content <- field(tensor, "content")[[1]]

  # in order to read, we first write the bytes to a file, then read with wav::read_wav
  tmp <- tempfile(fileext = ".wav")
  on.exit({file.remove(tmp)}, add = TRUE)
  writeBin(object = content[[1]], con = tmp)

  wav::read_wav(tmp)
}

#' @export
value.tfevents_summary_values_images <- function(x, ...) {
  # images are stored as tensors in the summary proto
  image <- field(x, "tensor")

  # the stored tensor is a 1d vector with 3 elements.
  # first 2 describe the dimension of the image, the third
  # contains a PNG encoded image.
  buffer <- field(image, "content")[[1]]
  dim1 <- readBin(buffer[[1]], what = integer(), size = 1)
  dim2 <- readBin(buffer[[2]], what = integer(), size = 1)
  img <- png::readPNG(buffer[[3]])

  if (!identical(dim(img)[1:2], c(dim1, dim2))) {
    cli::cli_abort("An error ocurred. Report a issue in the tfevents repository.")
  }

  img
}

#' @export
value.tfevents_summary_values_text <- function(x, ...) {
  tensor <- field(x, "tensor")
  rawToChar(field(tensor, "content")[[1]][[1]])
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
