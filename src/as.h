#include <Rcpp.h>
#include "generated/event.pb.h"
#include "utils.h"

template <>
std::vector<tensorboard::SummaryMetadata> Rcpp::as<std::vector<tensorboard::SummaryMetadata>> (SEXP x) {
  auto r_summary_metadata = Rcpp::as<Rcpp::List>(x);

  auto r_plugin_name = Rcpp::as<std::vector<std::string>>(r_summary_metadata["plugin_name"]);
  auto r_display_name = Rcpp::as<Rcpp::CharacterVector>(r_summary_metadata["display_name"]);
  auto r_description = Rcpp::as<Rcpp::CharacterVector>(r_summary_metadata["description"]);
  auto r_plugin_content = Rcpp::as<Rcpp::List>(r_summary_metadata["plugin_content"]);

  std::vector<tensorboard::SummaryMetadata> metadata;
  for (size_t i = 0; i < r_plugin_name.size(); i++) {
    tensorboard::SummaryMetadata meta;
    meta.mutable_plugin_data()->CopyFrom(make_plugin_data(r_plugin_name[i], r_plugin_content[i]));
    if (!Rcpp::CharacterVector::is_na(r_display_name[i])) {
      meta.set_display_name(r_display_name[i]);
    }
    if (!Rcpp::CharacterVector::is_na(r_description[i])) {
      meta.set_summary_description(r_description[i]);
    }
    metadata.push_back(meta);
  }

  return metadata;
}

template <>
std::vector<tensorboard::Summary_Image> Rcpp::as<std::vector<tensorboard::Summary_Image>> (SEXP x) {
  auto r_images = Rcpp::as<Rcpp::List>(x);

  auto r_buffer = Rcpp::as<Rcpp::List>(r_images["buffer"]);
  auto r_width =  Rcpp::as<Rcpp::IntegerVector>(r_images["width"]);
  auto r_height =  Rcpp::as<std::vector<int64_t>>(r_images["height"]);
  auto r_colorspace =  Rcpp::as<std::vector<int64_t>>(r_images["colorspace"]);

  std::vector<tensorboard::Summary_Image> images;
  for (size_t i =0; i < r_buffer.size(); i++) {
    tensorboard::Summary_Image img;
    // we abuse by setting height to -1 to indicate that image is NA
    img.set_height(-1);
    if (!Rcpp::IntegerVector::is_na(r_width[i])) {
      img.set_height(r_height[i]);
      img.set_width(r_width[i]);
      img.set_colorspace(r_colorspace[i]);

      // buffer is a blob object, that itself is a list of raw vectors.
      // here it should be list of a sigle element.
      auto buf = Rcpp::as<RawVector>(r_buffer[i]);

      img.set_encoded_image_string(std::string(buf.begin(), buf.end()));
    }
    images.push_back(img);
  }
  return images;
}

template <>
tensorboard::Summary Rcpp::as<tensorboard::Summary> (SEXP x) {
  auto r_summary = Rcpp::as<Rcpp::List>(x);

  const auto r_tag = Rcpp::as<std::vector<std::string>>(r_summary["tag"]);
  const auto r_metadata = Rcpp::as<std::vector<tensorboard::SummaryMetadata>>(r_summary["metadata"]);
  const auto r_value = Rcpp::as<Rcpp::NumericVector>(r_summary["value"]); // Use a numeric vector to allow NA's
  const auto r_image = Rcpp::as<std::vector<tensorboard::Summary_Image>>(r_summary["image"]);
  const auto r_tensor = Rcpp::as<std::vector<tl::optional<tensorboard::TensorProto>>>(r_summary["tensor"]);

  tensorboard::Summary summary;
  for (size_t i = 0; i < r_tag.size(); i++) {
    auto value = summary.add_value();
    value->set_tag(r_tag[i]);
    auto metadata = r_metadata[i];
    value->mutable_metadata()->CopyFrom(metadata);

    // If the value is NA, we don't save it
    if (!Rcpp::NumericVector::is_na(r_value[i])) {
      value->set_simple_value((float)r_value[i]);
    }

    // images can be NA, in this case we make ttheir height -1 when creating
    // tthe pb message
    auto image = r_image[i];
    if (image.height() > 0) {
      // // See also https://github.com/tensorflow/tensorboard/blob/a74c10dd197e7b2a07219855a61bc62651e80065/tensorboard/plugins/image/summary_v2.py#L104
      // value->set_tag("image_summary/" + r_tag[i] + "/image" + (r_tag.size() > 1 ? ("/" + std::to_string(i)) : ""));
      value->mutable_image()->CopyFrom(image);
    }

    if (r_tensor[i].has_value()) {
      value->mutable_tensor()->CopyFrom(r_tensor[i].value());
    }
  }
  return summary;
}

template <>
std::vector<tensorboard::Summary> Rcpp::as<std::vector<tensorboard::Summary>> (SEXP x) {
  auto r_summary = Rcpp::as<Rcpp::List>(x);
  std::vector<tensorboard::Summary> summary;
  for (size_t i = 0; i < r_summary.size(); i++) {
    summary.push_back(Rcpp::as<tensorboard::Summary>(r_summary[i]));
  }
  return summary;
}

template <>
std::vector<tensorboard::Event> Rcpp::as<std::vector<tensorboard::Event>> (SEXP x) {
  auto r_event = Rcpp::as<Rcpp::List>(x);

  auto r_wall_time = Rcpp::as<std::vector<std::int64_t>>(r_event["wall_time"]);
  auto r_step = Rcpp::as<std::vector<std::int64_t>>(r_event["step"]);
  auto r_summary = Rcpp::as<std::vector<tensorboard::Summary>>(r_event["summary"]);
  std::vector<tensorboard::Event> event;
  for (size_t i = 0; i < r_wall_time.size(); i++) {
    tensorboard::Event e;
    e.set_wall_time(r_wall_time[i]);
    e.set_step(r_step[i]);
    e.mutable_summary()->CopyFrom(r_summary[i]);
    event.push_back(e);
  }
  return event;
}
