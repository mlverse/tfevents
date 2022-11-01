map2 <- function(x, f, ...) {
  out <- vector(length = length(x))
  for(nm in names(x)) {
    out[[nm]] <- f(x[[nm]], nm, ...)
  }
  out
}
