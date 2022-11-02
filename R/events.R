#' Scalar event
#'
#' @export
event_scalar <- function(data, description = "", display_name = "") {
  new_event(list(
    data = data,
    description = description,
    display_name = display_name
  ), class = "event_scalar")
}


new_event <- function(data, class) {
  structure(data, class = c(class, "event"))
}
