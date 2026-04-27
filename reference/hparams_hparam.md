# Defines a HParam

Hparam object are used to describe names and domains of hyperparameters
so TensorBoard UI can show additional information about them.

## Usage

``` r
hparams_hparam(name, domain = NA, display_name = name, description = name)
```

## Arguments

- name:

  Name of the hyperparameter.

- domain:

  A list of values that can be assumed by the hyperparameter. It can be
  [`character()`](https://rdrr.io/r/base/character.html),
  [`numeric()`](https://rdrr.io/r/base/numeric.html) or
  [`logical()`](https://rdrr.io/r/base/logical.html) vector. You can
  also pass a named numeric vector with eg
  `c(min_value = 0, max_value = 10)` in this case, any value in this
  range is accepted.

- display_name:

  Display name of the hparameter for the TensorBoard UI. By default it's
  identical to the name.

- description:

  Parameter description. Shown in tooltips around the TensorBoard UI.

## Value

A `hparams_hparam` object.

## Note

A list of `hparam` values can be passed to
[`log_hparams_config()`](https://mlverse.github.io/tfevents/reference/log_hparams_config.md)
so you define the hyperparameters that are tracked by the experiment.

## Examples

``` r
hparams_hparam("optimizer", domain = c("adam", "sgd"))
#> $name
#> [1] "optimizer"
#> 
#> $domain
#> [1] "adam" "sgd" 
#> 
#> $display_name
#> [1] "optimizer"
#> 
#> $description
#> [1] "optimizer"
#> 
#> attr(,"class")
#> [1] "hparams_hparam"
hparams_hparam("num_units", domain = c(128, 512, 1024))
#> $name
#> [1] "num_units"
#> 
#> $domain
#> [1]  128  512 1024
#> 
#> $display_name
#> [1] "num_units"
#> 
#> $description
#> [1] "num_units"
#> 
#> attr(,"class")
#> [1] "hparams_hparam"
hparams_hparam("use_cnn", domain = c(TRUE, FALSE))
#> $name
#> [1] "use_cnn"
#> 
#> $domain
#> [1]  TRUE FALSE
#> 
#> $display_name
#> [1] "use_cnn"
#> 
#> $description
#> [1] "use_cnn"
#> 
#> attr(,"class")
#> [1] "hparams_hparam"
hparams_hparam("dropout", domain = c(min_value = 0, max_value = 0.5))
#> $name
#> [1] "dropout"
#> 
#> $domain
#> min_value max_value 
#>       0.0       0.5 
#> 
#> $display_name
#> [1] "dropout"
#> 
#> $description
#> [1] "dropout"
#> 
#> attr(,"class")
#> [1] "hparams_hparam"
```
