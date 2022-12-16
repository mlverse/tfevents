collect_events <- function(logdir = get_default_logdir()) {
  iter <- events_iterator(logdir)
  vec_rbind(!!!coro::collect(iter))
}

collect_summaries <- function(logdir = get_default_logdir()) {
  iter <- summaries_iterator(logdir)
  vec_rbind(!!!coro::collect(iter))
}

collect_scalars <- function(logdir = get_default_logdir()) {
  iter <- scalars_iterator(logdir)
  vec_rbind(!!!coro::collect(iter))
}

scalars_iterator <- function(logdir = get_default_logdir()) {
  iter <- summaries_iterator(logdir)
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

summaries_iterator <- function(logdir = get_default_logdir()) {
  rlang::check_installed("tidyr")
  iter <- events_iterator(logdir)
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
