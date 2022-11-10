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
  expect_equal(tags$images, "hello")
})

test_that("can write a ggplot", {
  gg <- ggplot2::qplot(1:10, 1:10, geom = "point")
  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = summary_image(gg))
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 2)

  buf <- field(field(events$summary[[2]], "image"), "buffer")
  reloaded <- png::readPNG(as.raw(buf[[1]]))

  expect_equal(dim(reloaded), c(2100, 2100, 3))
})
