#include <Rcpp.h>
#include "generated/event.pb.h"
#include "event_writer.h"

// [[Rcpp::export]]
bool write_events (std::vector<tensorboard::Event> events, Rcpp::List writers) {
  std::cout << "cast"<< std::endl;
  for(size_t i = 0; i < events.size(); i++) {
    auto writer = Rcpp::as<Rcpp::XPtr<EventWriter>>(writers[i]);
    auto event = events[i];
    writer->write_event(events[i]);
  }
  return true;
}
