#include <Rcpp.h>
#include "generated/event.pb.h"
#include "utils.h"

static auto pkg = Rcpp::Environment::namespace_env("tfevents");
static auto r_summary_metadata = Rcpp::Function(pkg["summary_metadata"]);
static auto r_summary_values = Rcpp::Function(pkg["summary_values"]);
static auto r_vec_c_list = Rcpp::Function(pkg["vec_c_list"]);
static auto r_event = Rcpp::Function(pkg["event"]);
static auto r_summary_summary_image = Rcpp::Function(pkg["summary_summary_image"]);
static auto r_tensor_proto = Rcpp::Function(pkg["tensor_proto"]);

template <>
SEXP Rcpp::wrap(const tensorboard::SummaryMetadata& object) {
  return r_summary_metadata(
    object.plugin_data().plugin_name(),
    object.display_name(),
    object.summary_description()
  );
}

template <>
SEXP Rcpp::wrap(const tensorboard::Summary_Image& object) {
  auto img_string = object.encoded_image_string();
  return r_summary_summary_image(
    Rcpp::Named("buffer", Rcpp::RawVector(img_string.begin(), img_string.end())),
    Rcpp::Named("width", object.width()),
    Rcpp::Named("height", object.height()),
    Rcpp::Named("colorspace", object.colorspace())
  );
}

SEXP tensor_proto_content (const tensorboard::TensorProto& object) {
  auto dtype = object.dtype();
  auto list = Rcpp::List();
  if (dtype == tensorboard::DataType::DT_FLOAT) {
    Rcpp::NumericVector out;
    for (int i =0; i < object.float_val_size(); i++) {
      out.push_back(object.float_val(i));
    }
    list.push_back(out);
  }
  else if (dtype == tensorboard::DataType::DT_DOUBLE) {
    Rcpp::NumericVector out;
    for (int i =0; i < object.double_val_size(); i++) {
      out.push_back(object.double_val(i));
    }
    list.push_back(out);
  }
  else if (dtype == tensorboard::DataType::DT_STRING) {
    Rcpp::List out;
    for (int i=0; i < object.string_val_size(); i++) {
      auto val = object.string_val(i);
      Rcpp::RawVector v(val.size());
      memcpy(&(v[0]), val.c_str(), v.size());
      out.push_back(v);
      out.attr("class") = std::vector<std::string>({"blob", "vctrs_list_of", "vctrs_vctr", "list"});
    }
    list.push_back(out);
  }
  else {
    Rcpp::stop("Can't read this object.");
  }
  return list;
}

template<>
SEXP Rcpp::wrap(const tensorboard::DataType& object) {
  switch (object) {
  case tensorboard::DataType::DT_FLOAT:
    return Rcpp::wrap("float");
  case tensorboard::DataType::DT_DOUBLE:
    return Rcpp::wrap("double");
  case tensorboard::DataType::DT_STRING:
    return Rcpp::wrap("string");
  default:
    Rcpp::stop("Can't read this type.");
  }
}

template<>
SEXP Rcpp::wrap(const tensorboard::TensorProto& object) {
  auto content = tensor_proto_content(object);
  auto shape = object.tensor_shape();
  auto shape_out = IntegerVector();

  for (int i = 0; i < shape.dim_size(); i++) {
    shape_out.push_back(shape.dim(i).size());
  }

  auto shape_out2 = Rcpp::List();
  shape_out2.push_back(shape_out);

  return r_tensor_proto(
    content,
    shape_out2,
    Rcpp::wrap(object.dtype())
  );
}

template <>
SEXP Rcpp::wrap(const tensorboard::Summary& object) {
  auto n_values = object.value_size();
  Rcpp::List summaries;
  for (size_t i = 0; i < n_values; i++) {
    auto value = object.value(i);

    summaries.push_back(r_summary_values(
      value.metadata(),
      value.tag(),
      Rcpp::Named("value", value.value_case() == tensorboard::Summary_Value::ValueCase::kSimpleValue  ? value.simple_value() : pkg["na"]),
      Rcpp::Named("image", value.value_case() == tensorboard::Summary_Value::ValueCase::kImage ? Rcpp::wrap(value.image()) : pkg["na"]),
      Rcpp::Named("tensor", value.value_case() == tensorboard::Summary_Value::ValueCase::kTensor ? Rcpp::wrap(value.tensor()): pkg["na"])
    ));
  }

  return r_vec_c_list(summaries);
}

template <>
SEXP Rcpp::wrap(const tensorboard::Event& object) {
  return r_event(
    Rcpp::Named("run", pkg["na"]),
    Rcpp::Named("wall_time", object.wall_time()),
    Rcpp::Named("step", object.step()),
    Rcpp::Named("summary", object.what_case() == tensorboard::Event::WhatCase::kSummary ? Rcpp::wrap(object.summary()) : pkg["na"]),
    Rcpp::Named("file_version", object.what_case() == tensorboard::Event::WhatCase::kFileVersion ? Rcpp::wrap(object.file_version()) : pkg["na"])
  );
}

template <>
SEXP Rcpp::wrap(const std::vector<tensorboard::Event>& object) {
  Rcpp::List events;
  for (size_t i = 0; i < object.size(); i++) {
    events.push_back(object[i]);
  }
  return r_vec_c_list(events);
}
