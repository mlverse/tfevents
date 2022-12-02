test_that("can write an audio file", {
  f <- wav::read_wav(test_path("resources/test-audio.wav"))
  audio <- array(t(f), dim = c(1, rev(dim(f))))

  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_audio(audio))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)

  # couldn't find a way to decode the binary string directly from memory,
  # so we write to disk and read again
  temp2 <- tempfile()
  w <- reticulate::import_builtins()$open(temp2, "wb")
  w$write(reader$tensors$value[[1]][[1]])

  f2 <- wav::read_wav(temp2)
  expect_true(all.equal(as.numeric(f), as.numeric(f2)))
})

test_that("can write multiple audio files from a array", {
  f <- wav::read_wav(test_path("resources/test-audio.wav"))
  f_t <- t(f)
  audio <- array(0, dim = c(10, rev(dim(f))))
  for (i in 1:10) {
    audio[i,,] <- f_t
  }

  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_audio(audio))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)

  for (i in 1:10) {
    temp2 <- tempfile()
    w <- reticulate::import_builtins()$open(temp2, "wb")
    w$write(reader$tensors$value[[1]][[i]])

    f2 <- wav::read_wav(temp2)
    expect_true(all.equal(as.numeric(f), as.numeric(f2)))
  }
})

test_that("can write directly from raw encoded file", {
  path <- test_path("resources/test-audio.wav")
  audio <- readBin(path, what = raw(), n = fs::file_info(path)$size)

  temp <- tempfile()
  with_logdir(temp, {
    log_event(x = summary_audio(audio))
  })

  skip_if_tbparse_not_available()
  reader <- tbparse$SummaryReader(temp)

  # couldn't find a way to decode the binary string directly from memory,
  # so we write to disk and read again
  temp2 <- tempfile()
  w <- reticulate::import_builtins()$open(temp2, "wb")
  w$write(reader$tensors$value[[1]][[1]])

  expect_true(all.equal(wav::read_wav(path), wav::read_wav(temp2)))
})

