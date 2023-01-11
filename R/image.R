#' Creates a image summary
#'
#' @param img An object that can be converted to an image.
#' @param ... Currently unused.
#' @inheritParams summary_scalar
#' @param width Width of the image.
#' @param height Height of the image.
#' @param colorspace Valid colorspace values are
#'   `1 - grayscale`,
#'   `2 - grayscale + alpha`,
#'   `3 - RGB`,
#'   `4 - RGBA`,
#'   `5 - DIGITAL_YUV`,
#'   `6 - BGRA`
#' @family summary
#' @returns An image summary that can be logged with [log_event()].
#' @examples
#' tmp <- tempfile()
#' with_logdir(tmp, {
#'   summary_image(array(runif(100), dim = c(1,10, 10, 1)))
#' })
#' @export
summary_image <- function(img, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_image")
}

#' @describeIn summary_image Cretes an image summary from a ggplot2 graph object.
#'   The `...` will be forwarded to [grDevices::png()].
#' @export
summary_image.ggplot <- function(img, ..., width = 480, height = 480, metadata = NULL, tag = NA) {
  temp <- tempfile(fileext = ".png")
  on.exit({unlink(temp)}, add = TRUE)

  grDevices::png(filename = temp, width = width, height = height, ...)
  plot(img)
  grDevices::dev.off()

  sze <- fs::file_info(temp)$size
  raw <- readBin(temp, n = sze, what = "raw")

  summary_image(
    raw,
    width = width,
    height = height,
    colorspace = 4,
    metadata = metadata,
    tag = tag
  )
}

#' @describeIn summary_image Creates an image from an R array. The array should be
#'   numeric, with values between 0 and 1. Dimensions should be `(batch, height, width, channels)`.
#' @export
summary_image.array <- function(img, ..., metadata = NULL, tag = NA) {
  if (length(dim(img)) <= 3) {
    cli::cli_abort(c(
      "Expected an array with dimensions {.code (batch, height, width, channels)}",
      i = "Got an array with dimensions {.code ({paste(dim(img), collapse=', ')})}."
    ))
  }

  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "images")
  }

  if (!all(field(metadata, "plugin_name") == "images")) {
    cli::cli_abort(c(
      "Plugin name should be 'images'",
      x = "Got {.val {unique(field(metadata, 'plugin_name'))}}"
    ))
  }

  # See https://github.com/tensorflow/tensorboard/blob/a74c10dd197e7b2a07219855a61bc62651e80065/tensorboard/plugins/image/summary_v2.py#L111
  # for the implementation.
  # The images are converted to a character evctor, the first 2 elements being the
  # dimensions, and the others containing the image encoded a png.
  png_images <- apply(img, 1, function(x) {
     png::writePNG(x)
  }, simplify = FALSE)
  blob_images <- blob::new_blob(png_images)

  dims <- dim(img)
  dims <- blob::blob(as.raw(dims[3]), as.raw(dims[2]))

  blobs <- c(dims, blob_images)

  summary_tensor(
    blobs,
    dtype = "string",
    metadata = metadata,
    tag = tag
  )
}

#' @describeIn summary_image Creates an image from [blob::blob()] vctr of PNG encoded images,
#'   (eg using [png::writePNG()]). `width`, `height` and `colorspace` are recycled
#'   thus they can be a single scalar or a vector the same size of the images blob.
#' @export
summary_image.blob <- function(img, ..., width, height, colorspace, metadata = NULL, tag = NA) {
  c(img, width, height, colorspace) %<-% vec_recycle_common(img, width, height, colorspace)

  image <- summary_summary_image(
    buffer = img,
    width = width,
    height = height,
    colorspace = colorspace
  )
  new_summary_image(image, metadata = metadata, tag = tag)
}

#' @describeIn summary_image Creates an image from a png encoded image. Eg, created
#'   with [png::writePNG()]. In this case you need to provide `width`, `height` and
#'   `colorspace` arguments.
#' @export
summary_image.raw <- function(img, ..., width, height, colorspace, metadata = NULL, tag = NA) {
  summary_image(
    img = blob::blob(img),
    width = width,
    height = height,
    colorspace = colorspace,
    metadata = metadata,
    tag = tag
  )
}

new_summary_image <- function(img = new_summary_summary_image(), ..., metadata = NULL, tag = character()) {
  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "images")
  }
  summary_values(metadata = metadata, image = img, class = "tfevents_summary_image", tag = tag)
}

summary_summary_image <- function(buffer, width, height, colorspace) {
  new_summary_summary_image(
    buffer = vec_cast(buffer, blob()),
    width = width,
    height = height,
    colorspace = colorspace
  )
}

#' @importFrom blob blob
new_summary_summary_image <- function(buffer = blob(), width = integer(), height = integer(), colorspace = integer()) {
  buffer <- vec_cast(buffer, blob())
  vctrs::new_rcrd(
    fields = list(
      buffer = buffer,
      width = width,
      height = height,
      colorspace = colorspace
    ),
    class = "summary_summary_image"
  )
}

#' @export
vec_ptype2.summary_summary_image.summary_summary_image <- function(x, y, ...) {
  new_summary_summary_image()
}
#' @export
vec_cast.summary_summary_image.summary_summary_image <- function(x, to, ...) {
  x
}
