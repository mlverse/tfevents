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
#'
#' @export
summary_image <- function(img, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_image")
}

#' @describeIn summary_image Cretes an image summary from a ggplot2 graph object.
#'   The `...` will be forwarded to [ggplot2::ggsave()].
#' @export
summary_image.ggplot <- function(img, ..., width = NA, height = NA, metadata = NULL, tag = NA) {
  temp <- tempfile(fileext = ".png")
  on.exit({unlink(temp)}, add = TRUE)

  txt <- utils::capture.output(type = "message", {
    ggplot2::ggsave(
      filename = fs::path_file(temp),
      path = fs::path_dir(temp),
      plot = img,
      units = "px",
      width = width,
      height = height,
      ...
    )
  })

  # there's no way to get the dimensions directly from the file
  # so we parse the message returned by `ggsave`.
  dims <- regmatches(txt,regexpr("[0-9x ]+",txt))
  dims <- strsplit(dims, "x")[[1]]
  dims <- as.integer(dims)

  sze <- fs::file_info(temp)$size
  raw <- readBin(temp, n = sze, what = "raw")

  summary_image(
    raw,
    width = dims[1],
    height = dims[2],
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

  buffers <- blob::new_blob(apply(img, 1, png::writePNG, simplify = FALSE))

  height <- dim(img)[2]
  width <- dim(img)[3]
  colorspace <- dim(img)[4]

  summary_image(
    buffers,
    height = height,
    width = width,
    colorspace = colorspace,
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
