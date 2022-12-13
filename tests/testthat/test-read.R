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

})
