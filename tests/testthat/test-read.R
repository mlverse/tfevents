test_that("collect float tensors", {
  temp <- tempfile()
  with_logdir(temp, {
   log_event(hello = summary_histogram(runif(1000)))
  })
  x <- collect_events(temp)

  expect_equal(nrow(x), 2)
})

test_that("can use the plugin function", {

  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = summary_histogram(runif(1000)))
    log_event(bye = 1)
  })

  summaries <- collect_summaries(temp)
  expect_equal(plugin(summaries$summary), c("histograms", "scalars"))
  expect_equal(summaries$plugin, c("histograms", "scalars"))

})

test_that("can iterate over events", {
  # tests low level reading functionality.
  # each event is read in a call to iterator.

  get_scalar_value <- function(value) {
    field(field(value, "summary")[[1]], "value")
  }

  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = 1)
  })

  iter <- events_iterator(temp)

  # reads the file definition event
  value <- iter()
  expect_true(inherits(value, "tfevents_event"))

  # reads the scalar
  value <- iter()
  expect_true(inherits(value, "tfevents_event"))
  expect_equal(get_scalar_value(value), 1)

  # no more events in the file, so exhausted is returned
  value <- iter()
  expect_true(is_exhausted(value))

  with_logdir(temp, {
    log_event(hello = 2)
  })

  # new events are written to the same file,
  # read them
  value <- iter()
  expect_true(inherits(value, "tfevents_event"))
  expect_equal(get_scalar_value(value), 2)

  # new events are written to a different file in the
  # directory and they are read.
  with_logdir(file.path(temp, "hello"), {
    log_event(hello = 3)
  })

  # first file event, is always a dummy event containg
  # timestamps and etc.
  value <- iter()
  expect_true(inherits(value, "tfevents_event"))

  value <- iter()
  expect_true(inherits(value, "tfevents_event"))
  expect_equal(get_scalar_value(value), 3)

  # no more events available, returns exhausted
  value <- iter()
  expect_true(is_exhausted(value))

})

test_that("can extract value", {

  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = 1)
    log_event(hello = 2)
    log_event(hist = summary_histogram(rnorm(1000)))
  })

  summaries <- collect_summaries(temp)
  expect_equal(value(summaries$summary[1]), 1)
  expect_error(value(summaries$summary), regexp = "single summary_value")


  histo <- value(summaries$summary[3])
  expect_true(is.data.frame(histo))
  expect_true(nrow(histo) == 30)
  expect_equal(names(histo), c("lower", "upper", "count"))

})
