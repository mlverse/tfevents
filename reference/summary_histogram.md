# Creates an histogram summary

Writes an histogram for later analysis in TensorBoard's Histograms and
Distributions tab.

## Usage

``` r
summary_histogram(data, ..., metadata = NULL, tag = NA)

# S3 method for class 'numeric'
summary_histogram(data, ..., metadata = NULL, tag = NA, buckets = 30)

# S3 method for class 'array'
summary_histogram(data, ..., metadata = NULL, tag = NA, buckets = 30)
```

## Arguments

- data:

  A Tensor of any shape. The histogram is computed over its elements,
  which must be castable to float64.

- ...:

  Currently unused. To allow future expansion.

- metadata:

  A `metadata` object, as created with
  [`summary_metadata()`](https://mlverse.github.io/tfevents/reference/summary_metadata.md).
  In most cases you don't need to change the default.

- tag:

  A tag that within the TensorBoard UI. See
  [`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md)
  for other ways of specifying the tag attribute.

- buckets:

  Optional positive int. The output will have this many buckets, except
  in two edge cases. If there is no data, then there are no buckets. If
  there is data but all points have the same value, then all buckets'
  left and right endpoints are the same and only the last bucket has
  nonzero count. Defaults to 30 if not specified.

## Value

An histogram summary that can be logged with
[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## Methods (by class)

- `summary_histogram(numeric)`: Creates an histogram summary for a
  numeric vector.

- `summary_histogram(array)`: Creates an histogram for array data.

## See also

Other summary:
[`summary_audio()`](https://mlverse.github.io/tfevents/reference/summary_audio.md),
[`summary_image()`](https://mlverse.github.io/tfevents/reference/summary_image.md),
[`summary_scalar()`](https://mlverse.github.io/tfevents/reference/summary_scalar.md),
[`summary_text()`](https://mlverse.github.io/tfevents/reference/summary_text.md)

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  for(i in 1:10) {
    log_event(x = summary_histogram(rnorm(10000)))
  }
})
```
