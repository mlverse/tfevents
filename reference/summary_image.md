# Creates a image summary

Creates a image summary

## Usage

``` r
summary_image(img, ..., metadata = NULL, tag = NA)

# S3 method for class 'ggplot'
summary_image(img, ..., width = 480, height = 480, metadata = NULL, tag = NA)

# S3 method for class 'array'
summary_image(img, ..., metadata = NULL, tag = NA)

# S3 method for class 'blob'
summary_image(img, ..., width, height, colorspace, metadata = NULL, tag = NA)

# S3 method for class 'raw'
summary_image(img, ..., width, height, colorspace, metadata = NULL, tag = NA)
```

## Arguments

- img:

  An object that can be converted to an image.

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

- width:

  Width of the image.

- height:

  Height of the image.

- colorspace:

  Valid colorspace values are `1 - grayscale`, `2 - grayscale + alpha`,
  `3 - RGB`, `4 - RGBA`, `5 - DIGITAL_YUV`, `6 - BGRA`

## Value

An image summary that can be logged with
[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## Methods (by class)

- `summary_image(ggplot)`: Cretes an image summary from a ggplot2 graph
  object. The `...` will be forwarded to
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html).

- `summary_image(array)`: Creates an image from an R array. The array
  should be numeric, with values between 0 and 1. Dimensions should be
  `(batch, height, width, channels)`.

- `summary_image(blob)`: Creates an image from
  [`blob::blob()`](https://blob.tidyverse.org/reference/blob.html) vctr
  of PNG encoded images, (eg using
  [`png::writePNG()`](https://rdrr.io/pkg/png/man/writePNG.html)).
  `width`, `height` and `colorspace` are recycled thus they can be a
  single scalar or a vector the same size of the images blob.

- `summary_image(raw)`: Creates an image from a png encoded image. Eg,
  created with
  [`png::writePNG()`](https://rdrr.io/pkg/png/man/writePNG.html). In
  this case you need to provide `width`, `height` and `colorspace`
  arguments.

## See also

Other summary:
[`summary_audio()`](https://mlverse.github.io/tfevents/reference/summary_audio.md),
[`summary_histogram()`](https://mlverse.github.io/tfevents/reference/summary_histogram.md),
[`summary_scalar()`](https://mlverse.github.io/tfevents/reference/summary_scalar.md),
[`summary_text()`](https://mlverse.github.io/tfevents/reference/summary_text.md)

## Examples

``` r
tmp <- tempfile()
with_logdir(tmp, {
  summary_image(array(runif(100), dim = c(1,10, 10, 1)))
})
#> <tfevents_summary_tensor[1]>
#> [1] <NA>
```
