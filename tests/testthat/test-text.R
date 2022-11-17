test_that("can log text values", {
  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_text("Hello world!"))
    log_event(x = "## hello world!\n hello!")
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(log_path = temp)

  expect_equal(nrow(reader$text), 2)
})

test_that("errors when passing more than one character value", {
  temp <- tempfile()
  expect_error(
    with_logdir(temp, {
      log_event(x = c("hello", "world"))
    }),
    regexp = "Can't log a character"
  )
  expect_error(
    with_logdir(temp, {
      log_event(x = summary_text(c("hello", "world")))
    }),
    regexp = "Can't log a character"
  )
})

test_that("make sure we fail when metadata is not text", {
  temp <- tempfile()
  expect_error(
    with_logdir(temp, {
      log_event(x = summary_text("hello", metadata = summary_metadata("text2")))
    }),
    regexp = "Plugin name should be "
  )
})
