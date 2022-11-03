map2 <- function(x, f, ...) {
  out <- vector(mode="list")
  for(nm in names(x)) {
    out[[nm]] <- f(x[[nm]], nm, ...)
  }
  out
}
