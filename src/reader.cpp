#include <Rcpp.h>
#include <fstream>
#include "generated/event.pb.h"
#include "reader.h"

EventFileIterator::EventFileIterator (const std::string& path) {
  file.open(path);
};

tensorboard::Event EventFileIterator::get_next () {
  std::uint64_t length;
  std::uint32_t crc;

  file.read(reinterpret_cast<char*>(&length), sizeof(std::uint64_t));

  if (file.eof()) {
    Rcpp::stop("File iterator is over.");
  }

  file.read(reinterpret_cast<char*>(&crc), sizeof(std::uint32_t));

  std::vector<char> buffer(length);
  file.read(&buffer[0], length);

  tensorboard::Event event;
  event.ParseFromString(std::string(buffer.begin(), buffer.end()));

  file.read(reinterpret_cast<char*>(&crc), sizeof(std::uint32_t));
  return event;
}

namespace tfevents {

Event::Event (const tensorboard::Event& e) {
  event.CopyFrom(e);
}

ImageImpl::ImageImpl (const tensorboard::Summary::Image& im) {
  img.CopyFrom(im);
}

Rcpp::Environment pkg = Rcpp::Environment::namespace_env("tfevents");
auto event_fn = Rcpp::Function(pkg["event"]);
auto r_summary_metadata = Rcpp::Function(pkg["summary_metadata"]);
auto r_tfevents_summary = Rcpp::Function(pkg["tfevents_summary"]);

Event::operator SEXP () const {
  // In general when this happens it means that this is the first event in files
  // that sate the wall time of first write + the file version
  if (event.has_file_version()) {
    return event_fn(
        "file_version",
        event.wall_time(),
        event.step(),
        Rcpp::Named("file_version", event.file_version())
    );
  }

  // This means that the current event is a summary event. The summary can be
  // a simple scalar, an image, a text, an audio or a tensor.
  if (event.has_summary()) {

    auto summary = event.summary();
    auto value = event.summary().value(0);
    auto summary_metadata = value.metadata();

    auto r_metadata = r_summary_metadata(
      Rcpp::Named("plugin_name", summary_metadata.plugin_data().plugin_name()),
      Rcpp::Named("display_name", summary_metadata.display_name()),
      Rcpp::Named("description", summary_metadata.summary_description())
    );

    std::string r_arg_name;
    SEXP r_value;
    if (value.has_simple_value()) {
      r_arg_name = "value";
      r_value = Rcpp::wrap(value.simple_value());
    } else if (value.has_image()) {
      r_arg_name = "image";
      r_value = Rcpp::wrap(ImageImpl(value.image()));
    } else {
      Rcpp::stop("Can't handle this argument type");
    }

    auto r_summary = r_tfevents_summary(
      r_metadata,
      r_value,
      Rcpp::Named(r_arg_name, r_value)
    );

    return event_fn(
      "summary",
      event.wall_time(),
      event.step(),
      Rcpp::Named("name", value.tag()),
      Rcpp::Named("summary", r_summary)
    );
  }

  Rcpp::stop("Can't handle this type of event");
}

auto r_new_image_impl = Rcpp::Function(pkg["new_image_impl"]);
ImageImpl::operator SEXP () const {
  auto img_string = img.encoded_image_string();
  return r_new_image_impl(
    Rcpp::Named("buffer", Rcpp::RawVector(img_string.begin(), img_string.end())),
    Rcpp::Named("width", img.width()),
    Rcpp::Named("height", img.height()),
    Rcpp::Named("colorspace", img.colorspace())
  );
}

}

// [[Rcpp::export]]
Rcpp::XPtr<EventFileIterator> create_event_file_iterator (const std::string& path) {
  return Rcpp::XPtr<EventFileIterator>(new EventFileIterator(path));
}

// [[Rcpp::export]]
tfevents::Event event_file_iterator_next (Rcpp::XPtr<EventFileIterator> iter) {
  return iter->get_next();
}

// [[Rcpp::export]]
std::vector<tfevents::Event> event_file_iterator_collect (const std::string& path) {
  auto iterator = EventFileIterator(path);
  std::vector<tfevents::Event> events;
  while (true) {
    try {
      events.push_back(iterator.get_next());
    } catch (...) {
      break;
    }
  }
  return events;
}
