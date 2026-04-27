# Package index

## Event logging

Functions directly related to event logging.

- [`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md)
  : Log event
- [`as_event()`](https://mlverse.github.io/tfevents/reference/as_event.md)
  : Coerce an object to a event.
- [`get_global_step()`](https://mlverse.github.io/tfevents/reference/get_global_step.md)
  [`set_global_step()`](https://mlverse.github.io/tfevents/reference/get_global_step.md)
  : Global step counters
- [`get_default_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
  [`set_default_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
  [`with_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
  [`local_logdir()`](https://mlverse.github.io/tfevents/reference/get_default_logdir.md)
  : Query and modify the logdir

## Summaries

Types of events that can be logged

- [`summary_audio()`](https://mlverse.github.io/tfevents/reference/summary_audio.md)
  : Summary audio
- [`summary_histogram()`](https://mlverse.github.io/tfevents/reference/summary_histogram.md)
  : Creates an histogram summary
- [`summary_image()`](https://mlverse.github.io/tfevents/reference/summary_image.md)
  : Creates a image summary
- [`summary_metadata()`](https://mlverse.github.io/tfevents/reference/summary_metadata.md)
  : Summary metadata
- [`summary_scalar()`](https://mlverse.github.io/tfevents/reference/summary_scalar.md)
  : Scalar event
- [`summary_text()`](https://mlverse.github.io/tfevents/reference/summary_text.md)
  : Creates a text summary

## HParams

Related to logging hyperparameters

- [`hparams_hparam()`](https://mlverse.github.io/tfevents/reference/hparams_hparam.md)
  : Defines a HParam
- [`hparams_metric()`](https://mlverse.github.io/tfevents/reference/hparams_metric.md)
  : Defines a Metric
- [`log_hparams()`](https://mlverse.github.io/tfevents/reference/log_hparams.md)
  [`summary_hparams()`](https://mlverse.github.io/tfevents/reference/log_hparams.md)
  : Log hyperaparameters
- [`log_hparams_config()`](https://mlverse.github.io/tfevents/reference/log_hparams_config.md)
  [`summary_hparams_config()`](https://mlverse.github.io/tfevents/reference/log_hparams_config.md)
  : Logs hyperparameters configuration

## Reading

Related to reading tfevents record files

- [`collect_events()`](https://mlverse.github.io/tfevents/reference/collect_events.md)
  [`events_logdir()`](https://mlverse.github.io/tfevents/reference/collect_events.md)
  : Collect data from tfevents records
- [`value()`](https://mlverse.github.io/tfevents/reference/value.md) :
  Extracts the value of a summary value
