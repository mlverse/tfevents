skip_if(inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error"))

tbparse <- reticulate::import("tbparse")

test_that("simple hparams experiment", {

  hparams <- list(
    hparams_hparam("dropout", domain = c(0.1, 0.5)),
    hparams_hparam("optimizer", domain = c("adam", "sgd"))
  )

  metrics <- list(
    hparams_metric(tag = "loss")
  )

  temp <- tempfile()
  with_logdir(temp, {
    hparams_config(hparams, metrics)
    hparams_hparams(hparams = list(dropout = 0.1, optimizer = "adam"))

    for (i in 1:10) {
      log_event(loss = runif(1))
    }
  })

  reader <- tbparse$SummaryReader(temp)

  expect_equal(reader$hparams$tag, c("dropout", "optimizer"))
  expect_equal(reader$hparams$value, list(0.1, "adam"))
  expect_equal(nrow(reader$scalars), 10)

  expect_equal(nrow(collect_events(temp)), 10 + 3)
  expect_equal(nrow(collect_summaries(temp)), 10 + 2)
  expect_equal(nrow(collect_scalars(temp)), 10)
})
