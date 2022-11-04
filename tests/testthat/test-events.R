test_that("events", {
  ev <- as_tfevent(1, wall_time = NA, step = 1, name = ".")
  expect_equal(length(ev), 1)

  x <- list(
    hello = 1,
    train = list(hello = 2, bye = summary_scalar(3)),
    valid = list(hello = 4, img = summary_image(array(0, dim = c(28, 28, 1))))
  )
  events <- as_tfevent(x, wall_time = 1L, step = 1L)
  expect_equal(length(events), 5)
})

test_that("test reflection", {

  ev <- as_tfevent(1, wall_time = NA, step = 1, name = ".")
  testfun(ev)

})
