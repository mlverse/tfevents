skip_if_tbparse_not_available <- function() {
  skip_if(inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error"))
}

if (inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error")) {
  tbparse <- NULL
} else {
  tbparse <- reticulate::import("tbparse")
}
