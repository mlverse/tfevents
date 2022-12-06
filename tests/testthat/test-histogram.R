test_that("can write and visualize histograms", {

  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(x = summary_histogram(rnorm(10000)))
    }
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)
  x <- reader$tensors
  expect_equal(nrow(x), 10)

  # test for arrays.
  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(x = summary_histogram(array(rnorm(10000), dim = c(10, 10, 100))))
    }
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)
  x <- reader$tensors
  expect_equal(nrow(x), 10)
})

test_that("edge case where there's no data doesn't fail", {
  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(x = summary_histogram(numeric(0)))
    }
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)
  x <- reader$tensors
  expect_equal(nrow(x), 10)

  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(x = summary_histogram(rep(0, 100)))
    }
  })

  reader <- tbparse$SummaryReader(temp)
  x <- reader$tensors
  expect_equal(nrow(x), 10)
})
