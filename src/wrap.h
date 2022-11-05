#include <Rcpp.h>
#include "generated/event.pb.h"
#include "utils.h"

static auto pkg = Rcpp::Environment::namespace_env("tfevents");
static auto r_summary_metadata = Rcpp::Function(pkg["summary_metadata"]);
static auto r_summary_values = Rcpp::Function(pkg["summary_values"]);
static auto r_vec_c_list = Rcpp::Function(pkg["vec_c_list"]);
static auto r_event = Rcpp::Function(pkg["event"]);
static auto r_summary_summary_image = Rcpp::Function(pkg["summary_summary_image"]);

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

template <>
SEXP Rcpp::wrap(const tensorboard::Summary& object) {
  auto n_values = object.value_size();
  Rcpp::List summaries;
  for (size_t i = 0; i < n_values; i++) {
    auto value = object.value(i);

    summaries.push_back(r_summary_values(
      value.metadata(),
      value.tag(),
      Rcpp::Named("value", value.has_simple_value() ? value.simple_value() : pkg["na"]),
      Rcpp::Named("image", value.has_image() ? Rcpp::wrap(value.image()) : pkg["na"])
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
    Rcpp::Named("summary", object.has_summary() ? Rcpp::wrap(object.summary()) : pkg["na"]),
    Rcpp::Named("file_version", object.has_file_version() ? Rcpp::wrap(object.file_version()) : pkg["na"])
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
