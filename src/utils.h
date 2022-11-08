#include <chrono>
#include "generated/plugins/scalar/plugin_data.pb.h"
#include "generated/summary.pb.h"
#include <Rcpp.h>
#include "generated/plugins/hparams/plugin_data.pb.h"

long get_wall_time ();
tensorboard::SummaryMetadata::PluginData make_plugin_data (std::string plugin_name,
                                                           SEXP plugin_content);
