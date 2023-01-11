#' Creates an histogram summary
#'
#' Writes an histogram for later analysis in TensorBoard's Histograms and
#' Distributions tab.
#'
#' @param data A Tensor of any shape. The histogram is computed over its elements,
#'   which must be castable to float64.
#' @inheritParams summary_scalar
#' @param buckets Optional positive int. The output will have this many buckets,
#'   except in two edge cases. If there is no data, then there are no buckets.
#'   If there is data but all points have the same value, then all buckets' left
#'   and right endpoints are the same and only the last bucket has nonzero count.
#'   Defaults to 30 if not specified.
#'
#' @returns
#' An histogram summary that can be logged with [log_event()].
#' @family summary
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   for(i in 1:10) {
#'     log_event(x = summary_histogram(rnorm(10000)))
#'   }
#' })
#'
#' @export
summary_histogram <- function(data, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_histogram")
}

#' @describeIn summary_histogram Creates an histogram summary for a numeric vector.
#' @export
summary_histogram.numeric <- function(data, ..., metadata = NULL, tag = NA, buckets = 30) {
  # this is mostly a literal translation from code in:
  # https://github.com/tensorflow/tensorboard/blob/2cd515880ea26ec66cfa85fbb38ad96cc38f6985/tensorboard/plugins/histogram/summary_v2.py#L41
  if (buckets == 0 || length(data) == 0) {
    histogram_buckets <- array(0, dim = c(buckets, 3))
  } else {
    min_ <- min(data)
    max_ <- max(data)
    range_ <- max_ - min_
    if (range_ == 0) {
      left_edges <- right_edges <- rep(min_, buckets)
      bucket_counts <- c(rep(0, buckets-1), length(data))
      histogram_buckets <- cbind(left_edges, right_edges, bucket_counts)
    } else {
      bucket_width <- range_ / buckets
      offsets <- data - min_
      bucket_indices <- floor(offsets / bucket_width)
      clamped_indices <- pmin(bucket_indices, buckets)
      bucket_counts <- sapply(seq(0, buckets-1), function(i) sum(clamped_indices==i))
      edges <- seq(min_, max_, length.out = buckets + 1)
      left_edges <- utils::head(edges, buckets)
      right_edges <- utils::tail(edges, buckets)
      histogram_buckets <- cbind(left_edges, right_edges, bucket_counts)
    }
  }

  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "histograms")
  }

  summary_tensor(
    histogram_buckets,
    dtype = "double",
    metadata = metadata,
    tag = tag
  )
}

#' @describeIn summary_histogram Creates an histogram for array data.
#' @export
summary_histogram.array <- function(data, ..., metadata = NULL, tag = NA, buckets = 30) {
  summary_histogram(
    as.numeric(data),
    ...,
    metadata = metadata,
    tag = tag,
    buckets = buckets
  )
}

