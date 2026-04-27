# Logs hyperparameters configuration

Logs the hyperaparameter configuration for a HyperParameter tuning
experiment. It allows you to define the domain for each hyperparameters
and what are the metrics that should be shown in the TensorBoard UI,
along with configuring their display name and descriptions.

## Usage

``` r
log_hparams_config(hparams, metrics, time_created_secs = get_wall_time())

summary_hparams_config(hparams, metrics, time_created_secs = get_wall_time())
```

## Arguments

- hparams:

  A list of `hparams` objects as created by
  [`hparams_hparam()`](https://mlverse.github.io/tfevents/reference/hparams_hparam.md).

- metrics:

  A list of `metrics` objects as created by
  [`hparams_metric()`](https://mlverse.github.io/tfevents/reference/hparams_metric.md).
  These metrics will be tracked by TensorBoard UI when displaying the
  hyperparameter tuning table.

- time_created_secs:

  The time the experiment is created in seconds since the UNIX epoch.

## Value

Invisibly returns the HParam conffuration data as a `summary` object.

## Functions

- `summary_hparams_config()`: For advanced users only. Creates a
  hyperaparameter configuration summary that can be logged with
  [`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## Recommendations

When loging hyperparameter tuning experiments, the log directory
organization is:

    - root:
     - log_hparams_config(...)
     - run1:
       - log_hparams(...)
       - log_event(...)
     - run2:
       - log_hparams(...)
       - log_event(...)

Ie you should have a root logdir that will only contain the
hyperaparameter config log, as created with `log_hparams_config()`. Then
each run in the experiment will have it's own logdir as a child
directory of the root logdir.

## See also

[`log_hparams()`](https://mlverse.github.io/tfevents/reference/log_hparams.md)
