test_that("Can't have untagged summaries", {

  temp <- tempfile()

  expect_error(with_logdir(temp, {
    log_event(1)
  }), regexp = "must have a tag")

  expect_error(with_logdir(temp, {
    log_event(summary_scalar(1))
  }), regexp = "must have a tag")

  expect_error(with_logdir(temp, {
    log_event(summary_scalar(1, tag = "hello"))
  }), regexp = NA)

  scalars <- collect_events(temp, type = "scalar")
  expect_equal(scalars$tag, "hello")

})
