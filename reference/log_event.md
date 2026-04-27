# Log event

Log event

## Usage

``` r
log_event(..., step = get_global_step(increment = TRUE))
```

## Arguments

- ...:

  Named values that you want to log. They can be possibly nested, in
  this case, the enclosing names are considered 'run' names by
  TensorBoard.

- step:

  The step associated the logs. If `NULL`, a managed step counter will
  be used, and the global step is increased in every call to
  `log_event()`.

## Value

Invisibly returns the logged data.

## Note

`log_event()` writes events to the default `logdir`. You can query the
default `logdir` with
[`get_default_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
and modify it with
[`set_default_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md).
You can also use the
[`with_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
context switcher to temporarily modify the logdir.

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  log_event(
     train = list(loss = runif(1), acc = runif(1)),
     valid = list(loss = runif(1), acc = runif(1))
  )
})
```
