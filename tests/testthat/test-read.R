test_that("collect float tensors", {

  temp <- tempfile()
  with_logdir(temp, {
   log_event(hello = summary_histogram(runif(1000)))
  })

  x <- collect_events(temp)

})
