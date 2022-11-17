test_that("can log text values", {

  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_text("Hello world!"))
    log_event(x = "## hello world!\n hello!")
  })

})
