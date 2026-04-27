# Creates events

We try to match events as closely as possible to the protobuf messages.
The hierarchy looks like:

    event (<event>):
     - run (<character>)
     - wall_time (<integer>)
     - step (<integer>)
     - summary (<summary> aka list_of<summary_values>):
        - values (list):
          - <summary_value>:
            - metadata (<summary_metadata>)
            - tag (<character>)
            - value (<numeric>)
            - image (<summary_summary_image>)
              - buffer (<blob>)
              - width (<integer>)
              - height (<integer>)
              - colorspace (<integer>)

## Usage

``` r
event(run, wall_time, step, ..., summary = NA, file_version = NA)
```
