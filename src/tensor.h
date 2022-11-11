#pragma once

#include <Rcpp.h>
#include "generated/tensor.pb.h"
#include "tl/optional.hpp"
#include "na.h"

template<>
std::vector<tl::optional<tensorboard::DataType>>
Rcpp::as<std::vector<tl::optional<tensorboard::DataType>>> (SEXP x) {
  const auto r_dtype = Rcpp::as<Rcpp::CharacterVector>(x);
  std::vector<tl::optional<tensorboard::DataType>> dtype;
  for (size_t i = 0; i < r_dtype.size(); i++) {
    auto r_dp = r_dtype[i];

    if (Rcpp::CharacterVector::is_na(r_dp)) {
      dtype.push_back(tl::nullopt);
      continue;
    }

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
  auto r_dims_name = Rcpp::CharacterVector(0);
  const auto no_names = Rf_isNull(r_dims.names());
  if (!no_names) {
    r_dims_name = r_dims.names();
  }
  std::vector<tensorboard::TensorShapeProto_Dim> dims;
  for (size_t i = 0; i < r_dims.size(); i++) {
    tensorboard::TensorShapeProto_Dim dim;
    dim.set_size(r_dims[i]);

    if ((!no_names) && (!Rcpp::CharacterVector::is_na(r_dims_name[i]))) {
      dim.set_name(r_dims_name[i]);
    }

    dims.push_back(dim);
  }

  return dims;
}

template<>
std::vector<tl::optional<tensorboard::TensorShapeProto>>
Rcpp::as<std::vector<tl::optional<tensorboard::TensorShapeProto>>> (SEXP x) {

  auto r_shapes = Rcpp::as<Rcpp::List>(x);
  auto r_dims = Rcpp::as<Rcpp::List>(r_shapes["dim"]);

  std::vector<tl::optional<tensorboard::TensorShapeProto>> shapes;
  for (size_t i = 0; i < r_dims.size(); i++) {
    if (r_is_na(r_shapes)) {
      shapes.push_back(tl::nullopt);
      continue;
    }

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


void add_string_values (tensorboard::TensorProto* tensor, SEXP x) {
  if (Rf_inherits(x, "blob")) {
    // a blob is a list of raw vectors in R
    auto values = Rcpp::as<Rcpp::List>(x);
    for (auto val : values) {
      auto raw = Rcpp::as<Rcpp::RawVector>(val);
      tensor->add_string_val(std::string(raw.begin(), raw.end()));
    }
    return;
  }

  for (auto val : Rcpp::as<std::vector<std::string>>(x)) {
    tensor->add_string_val(val);
  }
}


template<>
std::vector<tl::optional<tensorboard::TensorProto>>
Rcpp::as<std::vector<tl::optional<tensorboard::TensorProto>>> (SEXP x) {

  auto r_tensors = Rcpp::as<Rcpp::List>(x);

  auto r_shape = Rcpp::as<std::vector<tl::optional<tensorboard::TensorShapeProto>>>(r_tensors["shape"]);
  auto r_dtype = Rcpp::as<std::vector<tl::optional<tensorboard::DataType>>>(r_tensors["dtype"]);
  auto r_content = Rcpp::as<Rcpp::List>(r_tensors["content"]);

  std::vector<tl::optional<tensorboard::TensorProto>> tensors;
  for (size_t i = 0; i < r_shape.size(); i++) {

    if (r_is_na(r_content[i])) {
      tensors.push_back(tl::nullopt);
      continue;
    }

    tensorboard::TensorProto tensor;

    tensor.set_dtype(r_dtype[i].value());
    tensor.mutable_tensor_shape()->CopyFrom(r_shape[i].value());
    SEXP content = r_content[i];
    switch(r_dtype[i].value()) {
    case tensorboard::DataType::DT_FLOAT:
      for (auto val : Rcpp::as<std::vector<float>>(content)) {
        tensor.add_float_val(val);
      }
      break;
    case tensorboard::DataType::DT_STRING:
      add_string_values(&tensor, content);
      break;
    case tensorboard::DataType::DT_DOUBLE:
      for (auto val : Rcpp::as<std::vector<double>>(content)) {
        tensor.add_double_val(val);
      }
      break;
    default:
      Rcpp::stop("Unsupported type");
    }

    tensors.push_back(tensor);
  }

  return tensors;
}
