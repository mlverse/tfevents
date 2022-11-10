test_that("write image", {
  temp <- tempfile()
  orig_img <- png::readPNG(test_path("resources/img.png"))
  img <- array(orig_img, dim = c(1, 28, 28, 1))

  with_logdir(temp, {
    log_event(hello = summary_image(img))
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 2)

  buf <- field(field(events$summary[[2]], "image"), "buffer")
  reloaded <- png::readPNG(as.raw(buf[[1]]))

  expect_equal(orig_img, reloaded)
})

test_that("can write nested images", {
  temp <- tempfile()
  orig_img <- png::readPNG(test_path("resources/img.png"))
  img <- array(orig_img, dim = c(1, 28, 28, 1))

  with_logdir(temp, {
    log_event(
      train = list(im = summary_image(img)),
      valid = list(im = summary_image(img))
    )
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 4)
})

test_that("can write a batch of images", {
  orig_img <- png::readPNG(test_path("resources/img.png"))
  img <- array(orig_img, dim = c(28, 28, 1))
  arr <- array(0, dim = c(10, 28, 28, 1))
  for(i in 1:10) {
    arr[i,,,] <- img
  }

  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = summary_image(arr))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)
  tags <- reader$tags
  expect_equal(as.character(tags$images), "hello")
  expect_equal(nrow(reader$images), 10)
})

test_that("can write a ggplot", {
  df <- data.frame(x = 1:10, y = 1:10)
  gg <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = summary_image(gg))
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 2)

  buf <- field(field(events$summary[[2]], "image"), "buffer")
  reloaded <- png::readPNG(as.raw(buf[[1]]))

  expect_equal(length(dim(reloaded)), 3)
})

test_that("error when passing an array with wrong dimensions", {
  orig_img <- png::readPNG(test_path("resources/img.png"))
  img <- array(orig_img, dim = c(28, 28, 1))
  temp <- tempfile()

  expect_error({
    with_logdir(temp, {
      log_event(hello = summary_image(img))
    })
  }, regexp = "array with dimensions")

})
