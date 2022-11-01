#include "record_writer.h"
#include "generated/event.pb.h"

// Prefix of version string present in the first entry of every event file.
static constexpr const char* kVersionPrefix = "brain.Event:";
static constexpr const int kCurrentVersion = 2;

class EventWriter {
public:
  std::unique_ptr<RecordWriter> record_writer;
  EventWriter(const std::string& file);
  ~EventWriter();
  bool write_event(const tensorboard::Event& event);
  void flush();
};
