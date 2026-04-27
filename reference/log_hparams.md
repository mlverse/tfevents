# Log hyperaparameters

Log hyperaparameters

## Usage

``` r
log_hparams(..., trial_id = NA, time_created_secs = get_wall_time())

summary_hparams(..., trial_id = NA, time_created_secs = get_wall_time())
```

## Arguments

- ...:

  Named values of hyperparameters.

- trial_id:

  A name for the current trail. by default it's the hash of the hparams
  names and values.

- time_created_secs:

  The time the experiment is created in seconds since the UNIX epoch.

## Value

A hyperparameter summary. USed for the side effects of logging the
hyperparameter to the logdir.

## Details

This function should only be called once in a logdir and it will record
the set of hyperparameters used in that run. Undefined behavior can
happen if it's called more than once in a logdir - specially how
TensorBoard behaves during visualization.

## Functions

- `summary_hparams()`: For advanced users only. It's recommended to use
  the `log_hparams()` function instead. Creates a hyperparameter summary
  that can be written with
  [`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## See also

[`log_hparams_config()`](https://mlverse.github.io/tfevents/reference/log_hparams_config.md)

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  log_hparams(optimizer = "adam", num_units = 16)
})
```
