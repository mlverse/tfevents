# Global step counters

Global step counters

## Usage

``` r
get_global_step(increment = TRUE)

set_global_step(step)
```

## Arguments

- increment:

  Wether to increment the `step` when getting it.

- step:

  New value for `step`.

## Value

The global step value for the default logdir, when `get_global_step`,
otherwise returns `NULL` invisibly.

## Details

`tfevents` tracks and automatically increased the step counter whenever
[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md)
is called. Note that, it maintains a separate step counter for each root
`logdir`, thus if you change the `logdir` using
[`set_default_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
or
[`with_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md),
a different step counter will be used.

## Functions

- `set_global_step()`: Set the global step.

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  print(get_global_step())
  set_global_step(100)
  print(get_global_step())
})
#> [1] 0
#> [1] 101
print(get_global_step())
#> [1] 0
```
