#' @export
write_event.summary_image <- function(data, name, step) {
  metadata <- field(data, "metadata")
  image <- field(data, "image")

  write_image(
    writer = get_writer(),
    name = name,
    step = step,
    buffer = as.raw(field(image, "buffer")[[1]]),
    width = field(image, "width"),
    height = field(image, "height"),
    depth = field(image, "colorspace"),
    description = field(metadata, "description"),
    display_name = field(metadata, "display_name")
  )
}

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
summary_image <- function(img, ..., metadata = NULL) {
  UseMethod("summary_image")
}

#' @describeIn summary_image Creates an image from an R array. The array should be
#'   numeric, with values between 0 and 1.
#' @export
summary_image.array <- function(img, ..., metadata = NULL) {
  img_buf <- png::writePNG(img)
  height <- dim(img)[1]
  width <- dim(img)[2]
  colorspace <- dim(img)[3]
  summary_image(
    img_buf,
    height = height, width = width, colorspace = colorspace,
    metadata = metadata
  )
}

#' @describeIn summary_image Creates an image from a png encoded image. Eg, created
#'   with [png::writePNG()]. In this case you need to provide `width`, `height` and
#'   `colorspace` arguments.
#' @export
summary_image.raw <- function(img, ..., width, height, colorspace, metadata = NULL) {
  image <- summary_summary_image(
    buffer = img,
    width = width,
    height = height,
    colorspace = colorspace
  )
  new_summary_image(image, metadata = metadata)
}

new_summary_image <- function(img = new_summary_summary_image(), ..., metadata = NULL) {
  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "images")
  }
  summary_values(metadata = metadata, image = img, class = "summary_image")
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


function() {
  writer <- get_writer()
  value <- png::writePNG(png::readPNG("tests/testthat/resources/img.png"))
  write_image(
    writer,
    name = "helo",
    step = 2,
    buffer = value,
    width = 28,
    height = 28,
    depth = 1,
    description = "",
    display_name = ""
  )
}
