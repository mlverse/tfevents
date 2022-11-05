#include <Rcpp.h>
#include "generated/event.pb.h"
#include "event_writer.h"

// [[Rcpp::export]]
Rcpp::XPtr<EventWriter> event_writer (std::string file) {
  return Rcpp::XPtr<EventWriter>(new EventWriter(file));
}


// [[Rcpp::export]]
bool write_events (std::vector<tensorboard::Event> events, Rcpp::List writers) {
  for(size_t i = 0; i < events.size(); i++) {
    auto writer = Rcpp::as<Rcpp::XPtr<EventWriter>>(writers[i]);
    auto event = events[i];
    writer->write_event(events[i]);
  }
  return true;
}
