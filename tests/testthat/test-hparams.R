test_that("simple hparams experiment", {

  hparams <- list(
    hparams_hparam("dropout", domain = c(0.1, 0.5)),
    hparams_hparam("optimizer", domain = c("adam", "sgd"))
  )

  metrics <- list(
    hparams_metric(tag = "loss", group = "train"),
    hparams_metric(tag = "loss", group = "valid")
  )

  temp <- tempfile()
  with_logdir(temp, {
    hparams_config(hparams, metrics)
  })

  with_logdir(temp, {
    hparams_hparams(hparams = list(dropout = 0.1, optimizer = "adam"))
    log_event(train = list(loss = runif(1)), valid = list(loss = runif(1)))
    log_event(train = list(loss = runif(1)), valid = list(loss = runif(1)))
    log_event(train = list(loss = runif(1)), valid = list(loss = runif(1)))
  })

  expect_true(TRUE)
})
