#include <Rcpp.h>
#include "generated/plugins/hparams/plugin_data.pb.h"
#include "generated/plugins/hparams/api.pb.h"
#include "tl/optional.hpp"

template <>
std::vector<tensorboard::hparams::DataType> Rcpp::as<std::vector<tensorboard::hparams::DataType>> (SEXP x) {
    std::vector<tensorboard::hparams::DataType> out;
    auto r_data_types = Rcpp::as<std::vector<std::string>>(x);
    for (auto r_data_type : r_data_types) {
        if (r_data_type == "float64") {
            out.push_back(tensorboard::hparams::DataType::DATA_TYPE_FLOAT64);
        } else if (r_data_type == "string") {
            out.push_back(tensorboard::hparams::DataType::DATA_TYPE_STRING);
        } else if (r_data_type == "bool") {
            out.push_back(tensorboard::hparams::DataType::DATA_TYPE_BOOL);
        } else {
            Rcpp::stop("Unknown data type: %s", r_data_type);
        }
    }
    return out;
}

template <>
std::vector<tl::optional<google::protobuf::ListValue>>
Rcpp::as<std::vector<tl::optional<google::protobuf::ListValue>>> (SEXP x) {
    std::vector<tl::optional<google::protobuf::ListValue>> out;
    auto r_values = Rcpp::as<Rcpp::List>(x);
    for (auto r_value : r_values) {
        if (Rf_isNull(r_value)) {
            out.push_back(tl::nullopt);
        } else {
            google::protobuf::ListValue list_value;
            auto r_value_vec = Rcpp::as<std::vector<std::string>>(r_value);
            for (auto value : r_value_vec) {
                list_value.add_values()->set_string_value(value);
            }
            out.push_back(list_value);
        }
    }
    return out;
}

template <>
std::vector<tl::optional<tensorboard::hparams::Interval>>
Rcpp::as<std::vector<tl::optional<tensorboard::hparams::Interval>>> (SEXP x) {
    std::vector<tl::optional<tensorboard::hparams::Interval>> out;
    auto r_intervals = Rcpp::as<Rcpp::List>(x);

    auto r_min_value = Rcpp::as<Rcpp::NumericVector>(r_intervals["min_value"]);
    auto r_max_value = Rcpp::as<Rcpp::NumericVector>(r_intervals["max_value"]);
    for (size_t i = 0; i < r_min_value.size(); i++) {
      if (Rcpp::NumericVector::is_na(r_min_value[i])) {
          out.push_back(tl::nullopt);
      } else {
          tensorboard::hparams::Interval interval;
          interval.set_min_value(r_min_value[i]);
          interval.set_max_value(r_max_value[i]);
          out.push_back(interval);
      }
    }
    return out;
}

template <>
std::vector<tensorboard::hparams::HParamInfo> Rcpp::as<std::vector<tensorboard::hparams::HParamInfo>>(SEXP x) {
  auto r_hparams_info = Rcpp::as<Rcpp::List>(x);

  auto r_name = Rcpp::as<std::vector<std::string>>(r_hparams_info["name"]);
  auto r_display_name = Rcpp::as<std::vector<std::string>>(r_hparams_info["display_name"]);
  auto r_description = Rcpp::as<std::vector<std::string>>(r_hparams_info["description"]);
  auto r_type = Rcpp::as<std::vector<tensorboard::hparams::DataType>>(r_hparams_info["type"]);
  auto r_domain_discrete = Rcpp::as<std::vector<tl::optional<google::protobuf::ListValue>>>(r_hparams_info["domain_discrete"]);
  auto r_domain_interval = Rcpp::as<std::vector<tl::optional<tensorboard::hparams::Interval>>>(r_hparams_info["domain_interval"]);

  std::vector<tensorboard::hparams::HParamInfo> out;
  for (size_t i = 0; i < r_name.size(); i++) {
    tensorboard::hparams::HParamInfo hparam_info;
    hparam_info.set_name(r_name[i]);
    hparam_info.set_display_name(r_display_name[i]);
    hparam_info.set_description(r_description[i]);
    hparam_info.set_type(r_type[i]);
    if (r_domain_discrete[i].has_value()) {
      hparam_info.mutable_domain_discrete()->CopyFrom(r_domain_discrete[i].value());
    }
    if (r_domain_interval[i].has_value()) {
      hparam_info.mutable_domain_interval()->CopyFrom(r_domain_interval[i].value());
    }
    out.push_back(hparam_info);
  }
  return out;
}

template <>
std::vector<tensorboard::hparams::MetricName>
Rcpp::as<std::vector<tensorboard::hparams::MetricName>> (SEXP x) {
  auto r_metric_name = Rcpp::as<Rcpp::List>(x);

  auto r_tag = Rcpp::as<std::vector<std::string>>(r_metric_name["tag"]);
  auto r_group = Rcpp::as<std::vector<std::string>>(r_metric_name["group"]);

  std::vector<tensorboard::hparams::MetricName> out;
  for (size_t i = 0; i < r_tag.size(); i++) {
    tensorboard::hparams::MetricName metric_name;
    metric_name.set_tag(r_tag[i]);
    metric_name.set_group(r_group[i]);
    out.push_back(metric_name);
  }
  return out;
}

template <>
std::vector<tensorboard::hparams::DatasetType>
Rcpp::as<std::vector<tensorboard::hparams::DatasetType>> (SEXP x) {
  auto r_dataset_type = Rcpp::as<std::vector<std::string>>(x);
  std::vector<tensorboard::hparams::DatasetType> out;
  for (auto r_type : r_dataset_type) {
    if (r_type == "training") {
      out.push_back(tensorboard::hparams::DatasetType::DATASET_TRAINING);
    } else if (r_type == "validation") {
      out.push_back(tensorboard::hparams::DatasetType::DATASET_VALIDATION);
    } else {
      Rcpp::stop("Unknown dataset type: %s", r_type);
    }
  }
  return out;
}

template <>
std::vector<tensorboard::hparams::MetricInfo>
Rcpp::as<std::vector<tensorboard::hparams::MetricInfo>> (SEXP x) {
  auto r_metric_info = Rcpp::as<Rcpp::List>(x);

  auto r_name = Rcpp::as<std::vector<tensorboard::hparams::MetricName>>(r_metric_info["name"]);
  auto r_display_name = Rcpp::as<std::vector<std::string>>(r_metric_info["display_name"]);
  auto r_description = Rcpp::as<std::vector<std::string>>(r_metric_info["description"]);
  auto r_dataset_type = Rcpp::as<std::vector<tensorboard::hparams::DatasetType>>(r_metric_info["dataset_type"]);

  std::vector<tensorboard::hparams::MetricInfo> out;
  for (size_t i = 0; i < r_name.size(); i++) {
    tensorboard::hparams::MetricInfo metric_info;
    metric_info.mutable_name()->CopyFrom(r_name[i]);
    metric_info.set_display_name(r_display_name[i]);
    metric_info.set_description(r_description[i]);
    metric_info.set_dataset_type(r_dataset_type[i]);
    out.push_back(metric_info);
  }
  return out;
}

template <>
std::vector<tensorboard::hparams::Experiment>
Rcpp::as<std::vector<tensorboard::hparams::Experiment>> (SEXP x) {
  auto r_experiments = Rcpp::as<Rcpp::List>(x);

  auto r_name = Rcpp::as<std::vector<std::string>>(r_experiments["name"]);
  auto r_description = Rcpp::as<std::vector<std::string>>(r_experiments["description"]);
  auto r_user = Rcpp::as<std::vector<std::string>>(r_experiments["user"]);
  auto r_time_created_secs = Rcpp::as<std::vector<int64_t>>(r_experiments["time_created_secs"]);
  auto r_hparam_infos = Rcpp::as<Rcpp::List>(r_experiments["hparam_infos"]);
  auto r_metric_infos = Rcpp::as<Rcpp::List>(r_experiments["metric_infos"]);

  std::vector<tensorboard::hparams::Experiment> out;
  for (size_t i = 0; i < r_name.size(); i++) {
    tensorboard::hparams::Experiment experiment;

    experiment.set_name(r_name[i]);
    experiment.set_description(r_description[i]);
    experiment.set_user(r_user[i]);
    experiment.set_time_created_secs(r_time_created_secs[i]);

    auto hparam_infos = Rcpp::as<std::vector<tensorboard::hparams::HParamInfo>>(r_hparam_infos[i]);
    for (auto hparam_info : hparam_infos) {
      experiment.add_hparam_infos()->CopyFrom(hparam_info);
    }

    auto metric_infos = Rcpp::as<std::vector<tensorboard::hparams::MetricInfo>>(r_metric_infos[i]);
    for (auto metric_info : metric_infos) {
      experiment.add_metric_infos()->CopyFrom(metric_info);
    }

    out.push_back(experiment);
  }

  return out;
}

template <>
std::vector<tensorboard::hparams::HParamsPluginData>
Rcpp::as<std::vector<tensorboard::hparams::HParamsPluginData>> (SEXP x) {
  auto r_plugin_data = Rcpp::as<Rcpp::List>(x);

  auto r_version = Rcpp::as<std::vector<std::int64_t>>(r_plugin_data["version"]);
  auto r_experiment = Rcpp::as<std::vector<tensorboard::hparams::Experiment>>(r_plugin_data["experiment"]);

  std::vector<tensorboard::hparams::HParamsPluginData> out;
  for (size_t i = 0; i < r_version.size(); i++) {
    tensorboard::hparams::HParamsPluginData plugin_data;
    plugin_data.set_version(r_version[i]);
    plugin_data.mutable_experiment()->CopyFrom(r_experiment[i]);
    out.push_back(plugin_data);
  }

  return out;
}
