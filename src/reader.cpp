#include <Rcpp.h>
#include <fstream>
#include "generated/event.pb.h"
#include "reader.h"

// declare that we know how to cast events into R objects.
template <>
SEXP Rcpp::wrap(const tensorboard::Event& object);

static auto pkg = Rcpp::Environment::namespace_env("tfevents");
static auto r_fill_run_field = Rcpp::Function(pkg["fill_run_field"]);


EventFileIterator::EventFileIterator (const std::string& path, const std::string& run_name) {
  this->path = path;
  this->run_name = run_name;
}

tensorboard::Event EventFileIterator::get_next () {
  std::uint64_t length;
  std::uint32_t crc;

  if (!file.is_open()) {
    file.open(path, std::ios::binary);
    file.seekg(current_pos, std::ios::beg);
  }

  current_pos = file.tellg();

  if (file.peek() == EOF) {
    file.close();
    Rcpp::stop("File iterator is over.");
  }

  file.read(reinterpret_cast<char*>(&length), sizeof(std::uint64_t));

  if (file.eof()) {
    file.clear();
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

// [[Rcpp::export]]
Rcpp::XPtr<EventFileIterator> create_event_file_iterator (const std::string& path, const std::string& run_name) {
  return Rcpp::XPtr<EventFileIterator>(new EventFileIterator(path, run_name));
}

// [[Rcpp::export]]
SEXP event_file_iterator_next (Rcpp::XPtr<EventFileIterator> iter) {
  auto event = iter->get_next();
  return r_fill_run_field(event, iter->run_name);
}
