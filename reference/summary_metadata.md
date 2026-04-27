# Summary metadata

Creates a summary metadata that can be passed to multiple `summary_`
functions.

## Usage

``` r
summary_metadata(
  plugin_name,
  display_name = NA_character_,
  description = NA_character_,
  ...,
  plugin_content = NA
)
```

## Arguments

- plugin_name:

  The name of the TensorBoard plugin that might use the summary.

- display_name:

  Display name for the summary.

- description:

  A description of the summary.

- ...:

  Currently unused. For future expansion.

- plugin_content:

  An optional plugin content. Note that it will only be used if the C++
  function `make_plugin_data` is aware of `plugin_content` for the
  specified plugin name. For advanced use only.

## Value

A `summary_metadata` object.

## Examples

``` r
summary <- summary_scalar(1, metadata = summary_metadata("scalars"))
```
