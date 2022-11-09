map2 <- function(x, f, ...) {
  out <- vector(mode="list")
  nms <- names2(x)
  # because of unnamed object, and the possibility of duplicated names
  # we iterate via integer indexes and then re-add the names.
  for(i in seq_along(nms)) {
    out[[i]] <- f(x[[i]], nms[i], ...)
  }
  names(out) <- nms
  out
}

# Unnamed object will now have names="" for all obejcts.
# this is similar to the approach of `names(c(1, x = 2))`
# that makes the name for the first element an empty character.
names2 <- function(x) {
  if (is.null(names(x))) {
    return(rep("", length(x)))
  } else {
    names(x)
  }
}
