# Extracts the value of a summary value

Summaries are complicated objects because they reflect the Protobuf
object structure that are serialized in the tfevents records files. This
function allows one to easily query vaues from summaries and will
dispatch to the correct way to extract images, audio, text, etc from
summary values.

## Usage

``` r
value(x, ...)

# S3 method for class 'tfevents_summary_values'
value(x, ..., as_list = FALSE)
```

## Arguments

- x:

  A `tfevents_summary_values` object.

- ...:

  Currently unused. To allow future extension.

- as_list:

  A boolean indicating if the results should be returned in a list. The
  default is to return a single value. If you need to extract values
  from multiple summaries use `as_list = TRUE`.

## Value

Depending on the type of the summary it returns an image, audio, text or
scalar.

## Methods (by class)

- `value(tfevents_summary_values)`: Acess values from `summary_values`.

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  for(i in 1:5) {
    log_event(my_log = runif(1))
  }
})

# iterate over all events
summary <- collect_events(temp, n = 1, type = "summary")
value(summary$summary)
#> [1] 0.226317
```
