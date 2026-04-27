# Collect data from tfevents records

Collects all events of a kind in a single data.frame ready for analysis.

## Usage

``` r
collect_events(
  logdir = get_default_logdir(),
  n = NULL,
  type = c("any", "summary", "scalar")
)

events_logdir(logdir = get_default_logdir())
```

## Arguments

- logdir:

  The log directory that you want to query events from. Either a file
  path or a connection created with `events_logdir()`.

- n:

  The maximum number of events to read from the connection. If `NULL`
  then all events are read, the default is `NULL`.

- type:

  The kind of events that are to be read. By default all events are
  read. If a different type is specified, then the result can include
  other columns as well as more lines.

## Value

A `tibble` with the collected events.

## Functions

- `events_logdir()`: Creates a connection to a logdir that can be reused
  to read further events later.

## Examples

``` r
temp <- tempfile()
with_logdir(temp, {
  for(i in 1:5) {
    log_event(my_log = runif(1))
  }
})
# collect all events in files, including file description events
collect_events(temp)
#> # A tibble: 6 × 4
#>        event run    step    summary
#>   <tfvnts_v> <chr> <int> <tfvnts_s>
#> 1      <./0> .         0           
#> 2      <./0> .         0        [1]
#> 3      <./1> .         1        [1]
#> 4      <./2> .         2        [1]
#> 5      <./3> .         3        [1]
#> 6      <./4> .         4        [1]
# collect summaries in the logdir
collect_events(temp, type = "summary")
#> # A tibble: 5 × 6
#>        event run    step    summary tag    plugin 
#>   <tfvnts_v> <chr> <int> <tfvnts__> <chr>  <chr>  
#> 1      <./0> .         0   <my_log> my_log scalars
#> 2      <./1> .         1   <my_log> my_log scalars
#> 3      <./2> .         2   <my_log> my_log scalars
#> 4      <./3> .         3   <my_log> my_log scalars
#> 5      <./4> .         4   <my_log> my_log scalars
# collect only scalar events
collect_events(temp, type = "scalar")
#> # A tibble: 5 × 7
#>        event run    step    summary tag    plugin    value
#>   <tfvnts_v> <chr> <int> <tfvnts__> <chr>  <chr>     <dbl>
#> 1      <./0> .         0   <my_log> my_log scalars 0.0808 
#> 2      <./1> .         1   <my_log> my_log scalars 0.834  
#> 3      <./2> .         2   <my_log> my_log scalars 0.601  
#> 4      <./3> .         3   <my_log> my_log scalars 0.157  
#> 5      <./4> .         4   <my_log> my_log scalars 0.00740
```
