# Defines a Metric

Metric objects are passed to
[`log_hparams_config()`](https://mlverse.github.io/tfevents/reference/log_hparams_config.md)
in order to define the collection of scalars that will be displayed in
the HParams tab in TensorBoard.

## Usage

``` r
hparams_metric(
  tag,
  group = NA,
  display_name = tag,
  description = tag,
  dataset_type = NA
)
```

## Arguments

- tag:

  The tag name of the scalar summary that corresponds to this metric.

- group:

  An optional string listing the subdirectory under the session's log
  directory containing summaries for this metric. For instance, if
  summaries for training runs are written to events files in
  `ROOT_LOGDIR/SESSION_ID/train`, then `group` should be `"train"`.
  Defaults to the empty string: i.e., summaries are expected to be
  written to the session logdir.

- display_name:

  An optional human-readable display name.

- description:

  An optional Markdown string with a human-readable description of this
  metric, to appear in TensorBoard.

- dataset_type:

  dataset_type: Either `"training"` or `"validation`, or `NA`.

## Value

A `hparams_metric` object.

## Examples

``` r
hparams_metric("loss", group = "train")
#> $tag
#> [1] "loss"
#> 
#> $group
#> [1] "train"
#> 
#> $display_name
#> [1] "loss"
#> 
#> $description
#> [1] "loss"
#> 
#> $dataset_type
#> [1] "unknown"
#> 
#> attr(,"class")
#> [1] "hparams_metric"
hparams_metric("acc")
#> $tag
#> [1] "acc"
#> 
#> $group
#> [1] NA
#> 
#> $display_name
#> [1] "acc"
#> 
#> $description
#> [1] "acc"
#> 
#> $dataset_type
#> [1] "unknown"
#> 
#> attr(,"class")
#> [1] "hparams_metric"
```
