as_tensor_proto <- function(x, dtype = NA, ...) {
  if (rlang::is_na(x)) return(vec_cast(NA, new_tensor_proto()))
  UseMethod("as_tensor_proto")
}

as_tensor_proto.arrray <- function(x, dtype = NA, ...) {
  dims <- dim(x)
  names(dims) <- dimnames(x)
  tensor_proto(x, shape = new_tensor_shape(dim = list(dims)), dtype = dtype)
}

tensor_proto <- function(content, shape, dtype = NA) {
  if ((length(dtype) == 1) && is.na(dtype)) {
    dtype <- sapply(content, make_default_dtype)
  }
  new_tensor_proto(
    content = content,
    shape = shape,
    dtype = dtype
  )
}

make_default_dtype <- function(x) {
  if (is.numeric(x)) {
    return("float")
  }
  if (is.character(x)) {
    return("string")
  }
}

new_tensor_proto <- function(content = new_list_of(), shape = new_tensor_shape(), dtype = character()) {
  new_rcrd(
    fields = list(
      content = content,
      shape = shape,
      dtype = dtype
    ),
    class = "tensor_proto"
  )
}

new_tensor_shape <- function(dim = new_list_of(ptype = integer())) {
  new_rcrd(
    fields = list(
      dim = dim
    ),
    class = "tensor_shape"
  )
}
