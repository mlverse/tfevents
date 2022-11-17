#include "utils.h"
#include "hparams.h"
#include "generated/plugins/image/plugin_data.pb.h"
#include "generated/plugins/text/plugin_data.pb.h"

tensorboard::SummaryMetadata::PluginData make_plugin_data (std::string plugin_name,
                                                           SEXP plugin_content) {
  tensorboard::SummaryMetadata::PluginData plugin_data;
  plugin_data.set_plugin_name(plugin_name);

  if (r_is_na(plugin_content)) {
    if (plugin_name == "scalars") {
      auto content = tensorboard::ScalarPluginData();
      content.set_version(0);
      plugin_data.set_content(content.SerializeAsString());
    }
    if (plugin_name == "images") {
      auto content = tensorboard::ImagePluginData();
      content.set_version(0);
      plugin_data.set_content(content.SerializeAsString());
    }
    if (plugin_name == "text") {
      auto content = tensorboard::TextPluginData();
      content.set_version(0);
      plugin_data.set_content(content.SerializeAsString());
    }
  } else {
    if (plugin_name == "hparams") {
      auto content = Rcpp::as<tensorboard::hparams::HParamsPluginData>(plugin_content);
      plugin_data.set_content(content.SerializeAsString());
    }
  }

  return plugin_data;
}

// [[Rcpp::export]]
long get_wall_time () {
  return std::chrono::duration_cast<std::chrono::seconds>(
    std::chrono::system_clock::now().time_since_epoch()
  ).count();
}
