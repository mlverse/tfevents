#' @export
summary_histogram <- function(data, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_histogram")
}

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
      left_edges <- head(edges, buckets)
      right_edges <- tail(edges, buckets)
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
