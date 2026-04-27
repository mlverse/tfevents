# Scalar event

Scalar event

## Usage

``` r
summary_scalar(value, ..., metadata = NULL, tag = NA)
```

## Arguments

- value:

  A numeric scalar value to be logged.

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

## Value

A `<scalar_event>` object.

## See also

Other summary:
[`summary_audio()`](https://mlverse.github.io/tfevents/reference/summary_audio.md),
[`summary_histogram()`](https://mlverse.github.io/tfevents/reference/summary_histogram.md),
[`summary_image()`](https://mlverse.github.io/tfevents/reference/summary_image.md),
[`summary_text()`](https://mlverse.github.io/tfevents/reference/summary_text.md)

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  log_event(loss = summary_scalar(1))
})
```
