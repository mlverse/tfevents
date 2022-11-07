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
    for (auto r_interval : r_intervals) {
        if (Rf_isNull(r_interval)) {
            out.push_back(tl::nullopt);
        } else {
            tensorboard::hparams::Interval interval;
            auto r_interval_vec = Rcpp::as<Rcpp::List>(r_interval);
            interval.set_min_value(r_interval_vec["min_value"]);
            interval.set_max_value(r_interval_vec["max_value"]);
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
tensorboard::hparams::Experiment Rcpp::as<tensorboard::hparams::Experiment> (SEXP x) {
  auto r_experiment = Rcpp::as<Rcpp::List>(x);
  tensorboard::hparams::Experiment experiment;

  experiment.set_name(Rcpp::as<std::string>(r_experiment["name"]));
  experiment.set_description(Rcpp::as<std::string>(r_experiment["description"]));
  experiment.set_user(Rcpp::as<std::string>(r_experiment["user"]));
  experiment.set_time_created_secs(Rcpp::as<int64_t>(r_experiment["time_created_secs"]));

  auto r_hparam_infos = Rcpp::as<std::vector<tensorboard::hparams::HParamInfo>>(r_experiment["hparam_infos"]);
  for (auto hparam_info : r_hparam_infos) {
    experiment.add_hparam_infos()->CopyFrom(hparam_info);
  }

  auto r_metric_infos = Rcpp::as<std::vector<tensorboard::hparams::MetricInfo>>(r_experiment["metric_infos"]);
  for (auto metric_info : r_metric_infos) {
    experiment.add_metric_infos()->CopyFrom(metric_info);
  }

  return experiment;
}

template <>
tensorboard::hparams::HParamsPluginData Rcpp::as<tensorboard::hparams::HParamsPluginData> (SEXP x) {
  auto r_plugin_data = Rcpp::as<Rcpp::List>(x);

  tensorboard::hparams::HParamsPluginData plugin_data;
  plugin_data.set_version(Rcpp::as<int64_t>(r_plugin_data["version"]));
  plugin_data.mutable_experiment()->CopyFrom(
      Rcpp::as<tensorboard::hparams::Experiment>(r_plugin_data["experiment"])
  );
  return plugin_data;
}
