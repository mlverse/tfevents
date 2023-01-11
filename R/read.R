#' Collect data from tfevents records
#'
#' Collects all events of a kind in a single data.frame ready for analysis.
#'
#' @param logdir The log directory that you want to query events from. Either a
#'  file path or a connection created with [events_logdir()].
#' @param n The maximum number of events to read from the connection. If `NULL`
#'  then all events are read, the default is `NULL`.
#' @param type The kind of events that are to be read. By default all events are
#'  read. If a different type is specified, then the result can include other
#'  columns as well as more lines.
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
#' collect_events(temp, type = "summary")
#' # collect only scalar events
#' collect_events(temp, type = "scalar")
#'
#' @export
collect_events <- function(logdir = get_default_logdir(), n = NULL,
                           type = c("any", "summary", "scalar")) {
  logdir <- events_logdir(logdir)
  type <- rlang::arg_match(type)

  if (!is.null(n) && n <= 0) {
    cli::cli_abort(c(
      "{.arg n} must be positive or `NULL`",
      "{.val {n}} is <=0 and not allowed."
    ))
  }

  events <- list()
  repeat {
    event <- read_next(logdir, type = type)
    if (is_exhausted(event)) {
      break
    } else {
      events[[length(events) + 1]] <- event
      if (!is.null(n) && length(events) == n)
        break
    }
  }
  vec_rbind(!!!events)
}

#' @describeIn collect_events Creates a connection to a logdir that can be reused
#'   to read further events later.
#' @export
events_logdir <- function(logdir = get_default_logdir()) {
  if (inherits(logdir, "tfevents_logdir_connection"))
    return(logdir)
  new_events_logdir_connection(logdir)
}

new_events_logdir_connection <- function(logdir) {
  con <- structure(
    new.env(parent = emptyenv()),
    class = "tfevents_logdir_connection"
  )

  files <- fs::dir_ls(logdir, type = "file", regexp = ".*tfevents.*", recurse = TRUE)
  iterators <- create_iterators(files, logdir)

  con$logdir <- logdir
  con$iterators <- iterators
  con$files <- files

  con
}

# possibly includes new files that might have been created in the logdir, and
# could yield new values.
refresh_events_logdir_connection <- function(con) {
  # if exhausted, maybe there's a new file in the directory that we were not
  # tracking yet, so we try it before returing the exhausted flag.
  # check if new files were added to the directory
  new_files <- fs::dir_ls(con$logdir, type = "file", regexp = ".*tfevents.*", recurse = TRUE)
  new_files <- new_files[!new_files %in% con$files]

  # append to files and iterators
  con$files <- c(con$files, new_files)
  con$iterators <- append(con$iterators, create_iterators(new_files, con$logdir))
  invisible(NULL)
}

read_next <- function(con, type) {
  if (type == "any") {
    read_next_event(con)
  } else if (type == "summary") {
    read_next_summary(con)
  } else if (type == "scalar") {
    read_next_scalar(con)
  }
}

read_next_event <- function(con) {
  out <- try_iterators(con$iterators)
  if (is_exhausted(out)) {
    refresh_events_logdir_connection(con)
    out <- try_iterators(con$iterators)
  }
  out
}

read_next_summary <- function(con) {
  out <- read_next_event(con)
  if (is_exhausted(out)) {
    out
  } else if (is.na(out$summary)) {
    read_next_summary(con)
  } else {
    out <- tidyr::unnest(out, summary)
    out$tag <- field(out$summary, "tag")
    out$plugin <- plugin(out$summary)
    out
  }
}

read_next_scalar <- function(con) {
  out <- read_next_summary(con)
  if (is_exhausted(out)) {
    out
  } else if (out$plugin == "scalars") {
    out$value <- value(out$summary)
    out
  } else {
    read_next_scalar(con)
  }
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
#' summary <- collect_events(temp, n = 1, type = "summary")
#' value(summary$summary)
#'
#' @export
value <- function(x, ...) {
  UseMethod("value")
}

#' @describeIn value Acess values from `summary_values`.
#' @param as_list A boolean indicating if the results should be returned in a list.
#'   The default is to return a single value. If you need to extract values from
#'   multiple summaries use `as_list = TRUE`.
#' @export
value.tfevents_summary_values <- function(x, ..., as_list = FALSE) {
  if (!vec_size(x) == 1 && !as_list) {
    cli::cli_abort(c(
      "You must pass a single summary_value to get it's value.",
      i = "Got size {.val {vec_size(x)}}",
      i = "Use {.val as_list = TRUE} to cast many values at once."
    ))
  }
  if (as_list) {
    lapply(x, value)
  } else {
    class(x) <- c(paste0("tfevents_summary_values_", plugin(x)), class(x))
    value(x)
  }
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
