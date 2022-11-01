#include "generated/event.pb.h"
#include "generated/summary.pb.h"
#include "plugins/scalar/plugin_data.pb.h"
#include "event_writer.h"
#include "generated/tensor.pb.h"
#include "generated/tensor_shape.pb.h"
#include "utils.h"

#include <Rcpp.h>

namespace scalar {

const auto PLUGIN_NAME = "scalars";
const auto PROTO_VERSION = 0;

tensorboard::SummaryMetadata create_summary_metadata (std::string display_name,
                                                      std::string description) {

  auto metadata = tensorboard::SummaryMetadata();
  metadata.set_display_name(display_name);
  metadata.set_summary_description(description);

  auto content = tensorboard::ScalarPluginData();
  content.set_version(PROTO_VERSION);

  auto plugin_data = new tensorboard::SummaryMetadata::PluginData();
  plugin_data->set_plugin_name(PLUGIN_NAME);
  plugin_data->set_content(content.SerializeAsString());

  metadata.set_allocated_plugin_data(plugin_data);

  return metadata;
}

}

namespace core {

tensorboard::Event event_scalar (const std::string& name, float data, int64_t step,
                                  const std::string& description) {

  auto shape = new tensorboard::TensorShapeProto();

  tensorboard::TensorProto tensor;
  tensor.add_float_val(data);
  tensor.set_dtype(tensorboard::DataType::DT_FLOAT);
  tensor.set_allocated_tensor_shape(shape);

  tensorboard::Event event;
  event.set_step(step);
  event.set_wall_time(get_wall_time());

  auto v = event.mutable_summary()->add_value();
  v->set_tag(name);
  v->set_simple_value(data);
  v->mutable_metadata()->CopyFrom(
      scalar::create_summary_metadata("", description)
  );

  return event;
}

}

// [[Rcpp::export]]
Rcpp::XPtr<EventWriter> event_writer (std::string file) {
  return Rcpp::XPtr<EventWriter>(new EventWriter(file));
}

// [[Rcpp::export]]
void flush_event_writer (Rcpp::XPtr<EventWriter> writer) {
  writer->flush();
}

// [[Rcpp::export]]
bool write_scalar (Rcpp::XPtr<EventWriter> writer, const std::string& name,
                                      float data, int64_t step,
                                      const std::string& description) {
  auto event = core::event_scalar(name, data, step, description);
  return writer->write_event(event);
}

// [[Rcpp::export]]
void unload_protobuf()
{
  google::protobuf::ShutdownProtobufLibrary();
}

