#pragma once

#include <Rcpp.h>
#include "generated/tensor.pb.h"

template<>
std::vector<tensorboard::DataType>
Rcpp::as<std::vector<tensorboard::DataType>> (SEXP x) {
  const auto r_dtype = Rcpp::as<std::vector<std::string>>(x);
  std::vector<tensorboard::DataType> dtype;
  for (size_t i = 0; i < r_dtype.size(); i++) {
    auto r_dp = r_dtype[i];

    if (r_dp == "float") {
      dtype.push_back(tensorboard::DataType::DT_FLOAT);
    }
    else if (r_dp == "string") {
      dtype.push_back(tensorboard::DataType::DT_STRING);
    }
    else if (r_dp == "double") {
      dtype.push_back(tensorboard::DataType::DT_DOUBLE);
    }
    else {
      Rcpp::stop("Unsupported data type.");
    }
  }
  return dtype;
}

template<>
std::vector<tensorboard::TensorShapeProto_Dim>
Rcpp::as<std::vector<tensorboard::TensorShapeProto_Dim>> (SEXP x) {
  const auto r_dims = Rcpp::as<NumericVector>(x);
  const auto r_dims_name = Rcpp::as<Rcpp::CharacterVector>(r_dims.names());
  std::vector<tensorboard::TensorShapeProto_Dim> dims;
  for (size_t i = 0; i < r_dims.size(); i++) {
    tensorboard::TensorShapeProto_Dim dim;
    dim.set_size(r_dims[i]);

    if (!Rcpp::CharacterVector::is_na(r_dims_name[i])) {
      dim.set_name(r_dims_name[i]);
    }

    dims.push_back(dim);
  }

  return dims;
}

template<>
std::vector<tensorboard::TensorShapeProto>
Rcpp::as<std::vector<tensorboard::TensorShapeProto>> (SEXP x) {

  auto r_shapes = Rcpp::as<Rcpp::List>(x);
  auto r_dims = Rcpp::as<Rcpp::List>(r_shapes["dims"]);

  std::vector<tensorboard::TensorShapeProto> shapes;
  for (size_t i = 0; i < r_dims.size(); i++) {
    tensorboard::TensorShapeProto shape;
    auto dims = Rcpp::as<std::vector<tensorboard::TensorShapeProto_Dim>>(r_dims[i]);
    for (size_t j = 0; j < dims.size(); j++) {
      auto dim = shape.add_dim();
      dim->CopyFrom(dims[j]);
    }
    shapes.push_back(shape);
  }

  return shapes;
}

template<>
std::vector<tensorboard::TensorProto>
Rcpp::as<std::vector<tensorboard::TensorProto>> (SEXP x) {

  auto r_tensors = Rcpp::as<Rcpp::List>(x);

  auto r_shape = Rcpp::as<std::vector<tensorboard::TensorShapeProto>>(r_tensors["shape"]);
  auto r_dtype = Rcpp::as<std::vector<tensorboard::DataType>>(r_tensors["dtype"]);
  auto r_content = Rcpp::as<Rcpp::List>(r_tensors["content"]);

  std::vector<tensorboard::TensorProto> tensors;
  for (size_t i = 0; i < r_shape.size(); i++) {
    tensorboard::TensorProto tensor;

    tensor.set_dtype(r_dtype[i]);
    tensor.mutable_tensor_shape()->CopyFrom(r_shape[i]);
    SEXP content = r_content[i];
    switch(r_dtype[i]) {
    case tensorboard::DataType::DT_FLOAT:
      for (auto val : Rcpp::as<std::vector<float>>(content)) {
        tensor.add_float_val(val);
      }
      break;
    case tensorboard::DataType::DT_STRING:
      for (auto val : Rcpp::as<std::vector<std::string>>(content)) {
        tensor.add_string_val(val);
      }
      break;
    default:
      Rcpp::stop("Unsupported type");
    }

    tensors.push_back(tensor);
  }

  return tensors;
}



