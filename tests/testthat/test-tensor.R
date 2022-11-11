test_that("can write a tensor", {
  skip_if_tbparse_not_available()

  tx <- array(0, dim = c(28, 28, 28))
  txc <- array("hello", dim = c(28, 28, 28))
  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_tensor(tx, "float"))
    log_event(y = summary_tensor(tx)) # auto detect type
    log_event(z = summary_tensor(txc)) # auto detect type
    log_event(a = summary_tensor(list(tx, txc))) # auto detect type
    log_event(b = summary_tensor(tx, "double"))
  })

  reader <- tbparse$SummaryReader(temp)
  df <- reader$tensors
  expect_equal(nrow(df), 6)
})

test_that("can write tensors with dimnames", {
  tx <- array(0, dim = c(1, 2, 2, 3))
  names(dim(tx)) <- c("b", "w", "h", "c")

  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_tensor(tx))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)

  df <- reader$tensors
  # TODO: can't easily test if dimension names we actually writen because
  # tbparse ignores them.
  expect_equal(dim(df$value[[1]]), c(1,2,2,3))
})

test_that("can set name within summary_tensor", {
  tx <- array(0, dim = c(1, 28, 28, 28))
  temp <- tempfile()
  with_logdir(temp, {
    log_event(summary_tensor(tx, tag = "hello"))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)
  expect_equal(reader$tags$tensors, "hello")
})



