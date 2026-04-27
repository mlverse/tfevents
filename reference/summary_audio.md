# Summary audio

Audio summaries can be played withing the TensorBoard UI.

## Usage

``` r
summary_audio(audio, ..., metadata = NULL, tag = NA)

# S3 method for class 'array'
summary_audio(audio, ..., sample_rate = 44100, metadata = NULL, tag = NA)

# S3 method for class 'raw'
summary_audio(audio, ..., metadata = NULL, tag = NA)

# S3 method for class 'blob'
summary_audio(audio, ..., metadata = NULL, tag = NA)
```

## Arguments

- audio:

  Object that will be written as an audio event in the tfevents record.

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

- sample_rate:

  The sample rate in Hz associated to the audio values.

## Value

An audio summary that can be logged with
[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md).

## Methods (by class)

- `summary_audio(array)`: Creates a summary from a 3D array with
  dimensions `(batch_size, n_samples, n_channels)`. Values must be in
  the range `[-1, 1]`.

- `summary_audio(raw)`: Creates an audio summary from a raw vector
  containing a WAV encoded audio file.

- `summary_audio(blob)`: Creates an audio summary from a blob (ie list
  of raw vectors) containing WAV encoded audio files.

## See also

Other summary:
[`summary_histogram()`](https://mlverse.github.io/tfevents/reference/summary_histogram.md),
[`summary_image()`](https://mlverse.github.io/tfevents/reference/summary_image.md),
[`summary_scalar()`](https://mlverse.github.io/tfevents/reference/summary_scalar.md),
[`summary_text()`](https://mlverse.github.io/tfevents/reference/summary_text.md)

## Examples

``` r
tmp <- tempfile()
with_logdir(tmp, {
  summary_audio(array(runif(100), dim = c(1,100, 1)))
})
#> <tfevents_summary_tensor[1]>
#> [1] <NA>
```
