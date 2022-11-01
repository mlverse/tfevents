#include "event_writer.h"
#include "utils.h"

EventWriter::EventWriter(const std::string& file) :
  record_writer(new RecordWriter(file)) {
  long time_in_seconds = get_wall_time();
  {
    // Write the first event with the current version, and flush
    // right away so the file contents will be easily determined.
    tensorboard::Event event;
    event.set_wall_time(time_in_seconds);
    event.set_file_version(std::string(kVersionPrefix) + std::to_string(kCurrentVersion));
    write_event(event);
  }
}

EventWriter::~EventWriter() {
  std::cout << "Deleting event" << std::endl;
}

bool EventWriter::write_event(const tensorboard::Event& event) {
  return this->record_writer->write_record(event.SerializeAsString());
}

void EventWriter::flush () {
  this->record_writer->flush();
}
