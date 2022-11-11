test_that("can write a tensor", {
  skip_if_tbparse_not_available()

  tx <- array(0, dim = c(28, 28, 28))
  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_tensor(tx, "float"))
  })

  reader <- tbparse$SummaryReader(temp)
  df <- reader$tensors
  expect_equal(nrow(df), 1)
})
