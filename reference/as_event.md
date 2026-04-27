# Coerce an object to a event.

Coerce an object to a event.

## Usage

``` r
as_event(x, step, wall_time, ...)
```

## Arguments

- x:

  Object that will be coerced to an event.

- step:

  The step that will be used when the event is logged. This is used by
  TensorBoard when showing data.

- wall_time:

  The all time the event will appended to the event. This field is used
  by TensorBoard when displaying information based on actual time.

- ...:

  currently unused.

## Value

A event vctr with class \<tfevents_event\>.

## Extending `as_event`

`as_event` is an S3 generic and you can implement method for your own
class. We don't export the `event` constructor though, so you should
implement it in terms of other `as_event` methods.

## Examples

``` r
as_event(list(hello = 1), step = 1, wall_time = 1)
#> <tfevents_event[1]>
#> [1] <./1>
```
