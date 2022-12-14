% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/events.R
\name{event}
\alias{event}
\title{Creates events}
\usage{
event(run, wall_time, step, ..., summary = NA, file_version = NA)
}
\description{
We try to match events as closely as possible to the protobuf messages.
The hierarchy looks like:

\if{html}{\out{<div class="sourceCode">}}\preformatted{event (<event>):
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
}\if{html}{\out{</div>}}
}
\keyword{internal}
