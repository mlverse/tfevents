#include "utils.h"

tensorboard::SummaryMetadata::PluginData make_plugin_data (std::string plugin_name) {
  tensorboard::SummaryMetadata::PluginData plugin_data;
  plugin_data.set_plugin_name(plugin_name);

  if (plugin_name == "scalars") {
    auto content = tensorboard::ScalarPluginData();
    content.set_version(0);
    plugin_data.set_content(content.SerializeAsString());
  }

  return plugin_data;
}

// [[Rcpp::export]]
long get_wall_time () {
  return std::chrono::duration_cast<std::chrono::seconds>(
    std::chrono::system_clock::now().time_since_epoch()
  ).count();
}
