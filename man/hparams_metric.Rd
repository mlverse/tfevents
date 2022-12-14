% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hparams.R
\name{hparams_metric}
\alias{hparams_metric}
\title{Defines a Metric}
\usage{
hparams_metric(
  tag,
  group = NA,
  display_name = tag,
  description = tag,
  dataset_type = NA
)
}
\arguments{
\item{tag}{The tag name of the scalar summary that corresponds to this
metric.}

\item{group}{An optional string listing the subdirectory under the
session's log directory containing summaries for this metric.
For instance, if summaries for training runs are written to
events files in \code{ROOT_LOGDIR/SESSION_ID/train}, then \code{group}
should be \code{"train"}. Defaults to the empty string: i.e.,
summaries are expected to be written to the session logdir.}

\item{display_name}{An optional human-readable display name.}

\item{description}{An optional Markdown string with a human-readable
description of this metric, to appear in TensorBoard.}

\item{dataset_type}{dataset_type: Either \code{"training"} or \verb{"validation}, or
\code{NA}.}
}
\value{
A \code{hparams_metric} object.
}
\description{
Metric objects are passed to \code{\link[=log_hparams_config]{log_hparams_config()}} in order to define the
collection of scalars that will be displayed in the HParams tab in TensorBoard.
}
\examples{
hparams_metric("loss", group = "train")
hparams_metric("acc")
}
