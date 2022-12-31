skip_if_tbparse_not_available()

test_that("simple hparams experiment", {

  hparams <- list(
    hparams_hparam("dropout", domain = c(min_value = 0.1, max_value = 0.5)),
    hparams_hparam("optimizer", domain = c("adam", "sgd")),
    hparams_hparam("use_cnn", domain = c(TRUE, FALSE)),
    hparams_hparam("num_units", c(8, 12, 16))
  )

  metrics <- list(
    hparams_metric(tag = "loss"),
    hparams_metric(tag = "accuracy", group = "valid"),
    hparams_metric(tag = "f1", dataset_type = "training"),
    hparams_metric(tag = "f12", dataset_type = "validation")
  )

  temp <- tempfile()
  with_logdir(temp, {
    log_hparams_config(hparams, metrics)
    log_hparams(
      dropout = 0.1,
      optimizer = "adam",
      use_cnn = TRUE,
      num_units = 8
    )

    for (i in 1:10) {
      log_event(loss = runif(1), valid = list(accuracy = runif(1)),
                f1 = runif(1), f12 = runif(1))
    }
  })

  reader <- tbparse$SummaryReader(temp)

  expect_equal(reader$hparams$tag, c("dropout", "num_units", "optimizer", "use_cnn"))
  expect_equal(reader$hparams$value, list(0.1, 8, "adam", TRUE))
  expect_equal(nrow(reader$scalars), 40)

  expect_equal(nrow(collect_events(temp)), 40 + 4)
  expect_equal(nrow(collect_events(temp, type="summary")), 40 + 2)
  expect_equal(nrow(collect_events(temp, type="scalar")), 40)
})

test_that("multiple runs, each in a different logdir", {

  hparams <- list(
    hparams_hparam("dropout", domain = c(min_value = 0.1, max_value = 0.5)),
    hparams_hparam("optimizer", domain = c("adam", "sgd")),
    hparams_hparam("use_cnn", domain = c(TRUE, FALSE)),
    hparams_hparam("num_units", c(8, 12, 16))
  )

  metrics <- list(
    hparams_metric(tag = "loss"),
    hparams_metric(tag = "accuracy", group = "valid"),
    hparams_metric(tag = "f1", dataset_type = "training")
  )

  temp <- tempfile()

  with_logdir(temp, {
    log_hparams_config(hparams, metrics)
  })

  for(run in 1:10) {
    with_logdir(file.path(temp, paste0("run", run)), {
      log_hparams(
        dropout = runif(1, min = 0.1, max = 0.5),
        optimizer = sample(c("adam", "sgd"), 1),
        use_cnn = sample(c(TRUE, FALSE), 1),
        num_units = sample(c(8, 12, 16), 1)
      )

      for (i in 1:10) {
        log_event(loss = runif(1), valid = list(accuracy = runif(1)),
                  f1 = runif(1))
      }
    })
  }

  reader <- tbparse$SummaryReader(temp)
  expect_equal(nrow(reader$hparams), 40)
  expect_equal(nrow(collect_events(file.path(temp, "run1"))), 30 + 3)
})

test_that("write only hparams without the config", {

  temp <- tempfile()
  for(run in 1:10) {
    with_logdir(file.path(temp, paste0("run", run)), {
      log_hparams(
        dropout = runif(1, min = 0.1, max = 0.5),
        optimizer = sample(c("adam", "sgd"), 1),
        use_cnn = sample(c(TRUE, FALSE), 1),
        num_units = sample(c(8, 12, 16), 1)
      )

      for (i in 1:10) {
        log_event(loss = runif(1), valid = list(accuracy = runif(1)),
                  f1 = runif(1))
      }
    })
  }

  reader <- tbparse$SummaryReader(temp)
  expect_equal(nrow(reader$hparams), 40)

})

