# Creates a text summary

Creates a text summary

## Usage

``` r
summary_text(txt, ..., metadata = NULL, tag = NA)

# S3 method for class 'character'
summary_text(txt, ..., metadata = NULL, tag = NA)
```

## Arguments

- txt:

  An object that can be converted to a text.

- ...:

  Currently unused.

- metadata:

  A `metadata` object, as created with
  [`summary_metadata()`](https://mlverse.github.io/tfevents/reference/summary_metadata.md).
  In most cases you don't need to change the default.

- tag:

  A tag that within the TensorBoard UI. See
  [`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md)
  for other ways of specifying the tag attribute.

## Value

A summary that can be logged with
[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## Methods (by class)

- `summary_text(character)`: Creates a summary from a scalar character
  vector.

## See also

Other summary:
[`summary_audio()`](https://mlverse.github.io/tfevents/reference/summary_audio.md),
[`summary_histogram()`](https://mlverse.github.io/tfevents/reference/summary_histogram.md),
[`summary_image()`](https://mlverse.github.io/tfevents/reference/summary_image.md),
[`summary_scalar()`](https://mlverse.github.io/tfevents/reference/summary_scalar.md)

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  log_event(
    x = "hello world",
    y = summary_text("hello world")
  )
})
```
