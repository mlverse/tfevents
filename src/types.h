#include <Rcpp.h>
#include "generated/event.pb.h"
#include "utils.h"

template <>
std::vector<tensorboard::SummaryMetadata> Rcpp::as<std::vector<tensorboard::SummaryMetadata>> (SEXP x) {
  auto r_summary_metadata = Rcpp::as<Rcpp::List>(x);

  auto r_plugin_name = Rcpp::as<std::vector<std::string>>(r_summary_metadata["plugin_name"]);
  auto r_display_name = Rcpp::as<std::vector<std::string>>(r_summary_metadata["display_name"]);
  auto r_description = Rcpp::as<std::vector<std::string>>(r_summary_metadata["description"]);

  std::vector<tensorboard::SummaryMetadata> metadata;
  for (size_t i = 0; i < r_plugin_name.size(); i++) {
    tensorboard::SummaryMetadata meta;
    meta.mutable_plugin_data()->CopyFrom(make_plugin_data(r_plugin_name[i]));
    meta.set_display_name(r_display_name[i]);
    meta.set_summary_description(r_description[i]);
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
      auto buf = Rcpp::as<RawVector>(Rcpp::as<Rcpp::List>(r_buffer[i])[0]);

      img.set_encoded_image_string(std::string(buf.begin(), buf.end()));
    }
    images.push_back(img);
  }
  return images;
}

template <>
tensorboard::Summary Rcpp::as<tensorboard::Summary> (SEXP x) {
  auto r_summary = Rcpp::as<Rcpp::List>(x);

  auto r_tag = Rcpp::as<std::vector<std::string>>(r_summary["tag"]);
  auto r_metadata = Rcpp::as<std::vector<tensorboard::SummaryMetadata>>(r_summary["metadata"]);
  auto r_value = Rcpp::as<Rcpp::NumericVector>(r_summary["value"]); // Use a numeric vector to allow NA's
  auto r_image = Rcpp::as<std::vector<tensorboard::Summary_Image>>(r_summary["image"]);


  tensorboard::Summary summary;
  for (size_t i = 0; i < r_tag.size(); i++) {
    auto value = summary.add_value();
    value->set_tag(r_tag[i]);
    value->mutable_metadata()->CopyFrom(r_metadata[i]);

    // If the value is NA, we don't save it
    if (!R_IsNA(r_value[i])) {
      value->set_simple_value(r_value[i]);
    }

    // images can be NA, and thus they wont have a width on them
    auto image = r_image[i];
    if (image.height() == -1) {
      value->mutable_image()->CopyFrom(image);
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
  auto r_step = Rcpp::as<std::vector<std::int64_t>>(r_event["wall_time"]);
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
