test_that("write a few simple scalars", {
  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(hello = i^2)
    }
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 10 + 1)
  expect_equal(events$run, rep(".", 11))

  scalars <- collect_scalars(temp)
  expect_equal(nrow(events), 10 + 1)
  expect_equal(scalars$value, (1:10)^2)
  expect_equal(scalars$step, 0:9)
  expect_equal(scalars$name, rep("hello", 10))
})

test_that("write nested scalar for multiple runs", {
  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(
        train = list(loss = i, acc = i^2),
        valid = list(loss = i+1, acc = (i+1)^2)
      )
    }
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 4*10 + 2)
  expect_equal(unique(events$run), c("train", "valid"))

  scalars <- collect_scalars(temp)
  expect_equal(nrow(scalars), 4*10)
  expect_equal(unique(scalars$name), c("loss", "acc"))
  expect_true(all(unique(scalars$step) %in% 0:9))
})

test_that("can log manually created scalars directly", {

  temp <- tempfile()
  with_logdir(temp, {
    for(i in 1:10) {
      log_event(
        train = list(loss = i, acc = i^2),
        valid = list(loss = i+1, acc = (i+1)^2),
        test = list(loss2 = summary_scalar(i+2))
      )
    }
  })

  events <- collect_events(temp)
  expect_equal(nrow(events), 4*10 + 10 + 3)

  scalars <- collect_scalars(temp)
  expect_equal(scalars[scalars$name == "loss2",]$value, 1:10 + 2)
})

test_that("can log with a specified step", {

  temp <- tempfile()
  with_logdir(temp, {
    log_event(hello = 1)
    log_event(hello = 1)
    log_event(hello = 1)
    log_event(hello = 1, step = 100)
    log_event(hello = 1)
    log_event(bye = 1, step = get_global_step(increment = FALSE))
  })

  scalars <- collect_scalars(temp)
  expect_true(100 %in% scalars$step)
  expect_true(3 %in% scalars$step)
  expect_equal(scalars$step[scalars$name == "bye"], 3)
})

test_that("local_logdir", {
  temp1 <- tempfile()
  temp2 <- tempfile()
  f <- function() {
    local_logdir(temp2)
    get_default_logdir()
  }

  with_logdir(temp1, {
    expect_equal(temp1, get_default_logdir())
    expect_equal(temp2, f())
    expect_equal(temp1, get_default_logdir())
  })

})
