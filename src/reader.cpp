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

// [[Rcpp::export]]
Rcpp::XPtr<EventFileIterator> create_event_file_iterator (const std::string& path) {
  return Rcpp::XPtr<EventFileIterator>(new EventFileIterator(path));
}

// [[Rcpp::export]]
tensorboard::Event event_file_iterator_next (Rcpp::XPtr<EventFileIterator> iter) {
  return iter->get_next();
}

// [[Rcpp::export]]
std::vector<tensorboard::Event> event_file_iterator_collect (const std::string& path) {
  auto iterator = EventFileIterator(path);
  std::vector<tensorboard::Event> events;
  while (true) {
    try {
      events.push_back(iterator.get_next());
    } catch (...) {
      break;
    }
  }
  return events;
}
