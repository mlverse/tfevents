test_that("write image", {
  temp <- tempfile()
  orig_img <- png::readPNG(test_path("resources/img.png"))
  img <- array(img, dim = c(28, 28, 1))

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
  img <- array(img, dim = c(28, 28, 1))

  with_logdir(temp, {
    log_event(
      train = list(im = summary_image(img)),
      valid = list(im = summary_image(img))
    )
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 4)
})
