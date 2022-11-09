skip_if(inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error"))

tbparse <- reticulate::import("tbparse")

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
    hparams_metric(tag = "f1", dataset_type = "training")
  )

  temp <- tempfile()
  with_logdir(temp, {
    hparams_config(hparams, metrics)
    hparams_hparams(hparams = list(
      dropout = 0.1,
      optimizer = "adam",
      use_cnn = TRUE,
      num_units = 8)
    )

    for (i in 1:10) {
      log_event(loss = runif(1), valid = list(accuracy = runif(1)),
                f1 = runif(1))
    }
  })

  reader <- tbparse$SummaryReader(temp)

  expect_equal(reader$hparams$tag, c("dropout", "num_units", "optimizer", "use_cnn"))
  expect_equal(reader$hparams$value, list(0.1, 8, "adam", TRUE))
  expect_equal(nrow(reader$scalars), 30)

  expect_equal(nrow(collect_events(temp)), 30 + 4)
  expect_equal(nrow(collect_summaries(temp)), 30 + 2)
  expect_equal(nrow(collect_scalars(temp)), 30)
})

