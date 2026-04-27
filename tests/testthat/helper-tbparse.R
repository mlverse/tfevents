skip_if_tbparse_not_available <- function() {
  skip_if(inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error"))
}

skip_if_no_tensorflow <- function() {
  skip_on_cran()
  skip_if_not_installed("tensorflow")
  skip_if(inherits(try(reticulate::import("tensorflow"), silent = TRUE), "try-error"))
}

if (inherits(try(reticulate::import("tbparse"), silent = TRUE), "try-error")) {
  tbparse <- NULL
} else {
  tbparse <- reticulate::import("tbparse")
}
