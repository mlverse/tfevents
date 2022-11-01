test_that("can write a simple scalar", {
  tmp <- tempfile()
  writer <- event_writer(tmp)
  expect_true(
    write_scalar(writer, name = "loss", data = 1, step = 0, description = "loss")
  )
})
