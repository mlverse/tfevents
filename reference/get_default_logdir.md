# Query and modify the logdir

[`log_event()`](https://mlverse.github.io/tfevents/reference/log_event.md)
has a notion of default logdir, so you don't need to specify it at every
call. These functions allow you to query and the current logdir.

## Usage

``` r
get_default_logdir()

set_default_logdir(logdir = "logs")

with_logdir(logdir, code)

local_logdir(logdir, .env = parent.frame())
```

## Arguments

- logdir:

  The `logdir` that you want to set as default.

- code:

  Expressions that will be evaluated in a context with the `new`
  `logdir` as the default `logdir`.

- .env:

  Environment that controls scope of changes. For expert use only.

## Value

The `logdir` for `get_default_logdir()` otherwise invisibly returns
`NULL`

## Functions

- `set_default_logdir()`: Modifies the default `logdir`.

- `with_logdir()`: Temporarily modify the default `logdir`.

- `local_logdir()`: Temporarily modify thedefault `logdir`.

## Examples

``` r
temp <- tempfile()
get_default_logdir()
#> [1] "logs"
with_logdir(temp, {
 print(get_default_logdir())
})
#> [1] "/tmp/RtmpEWAzcP/file257510c55873"
```
