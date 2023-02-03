#pragma once

#include <Rcpp.h>
#include "generated/plugins/hparams/plugin_data.pb.h"
#include "generated/plugins/hparams/api.pb.h"
#include "tl/optional.hpp"
#include "na.h"

template <>
inline std::vector<tensorboard::hparams::DataType> Rcpp::as<std::vector<tensorboard::hparams::DataType>> (SEXP x) {
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

template<>
inline google::protobuf::Value
  Rcpp::as<google::protobuf::Value> (SEXP x) {
    google::protobuf::Value out;
    switch(TYPEOF(x)) {
    case REALSXP:
      out.set_number_value(Rcpp::as<double>(x));
      break;
    case STRSXP:
      out.set_string_value(Rcpp::as<std::string>(x));
      break;
    case LGLSXP:
      out.set_bool_value(Rcpp::as<bool>(x));
      break;
    default:
      Rcpp::stop("Unsupported type");
    }
    return out;
  }

template <>
inline std::vector<tl::optional<google::protobuf::ListValue>>
Rcpp::as<std::vector<tl::optional<google::protobuf::ListValue>>> (SEXP x) {
  std::vector<tl::optional<google::protobuf::ListValue>> out;
  auto r_values = Rcpp::as<Rcpp::List>(x);
  for (auto r_value : r_values) {
    if (Rf_isNull(r_value)) {
      out.push_back(tl::nullopt);
    } else {
      google::protobuf::ListValue list_value;
      auto r_value_vec = Rcpp::as<Rcpp::List>(r_value);
      for (auto value : r_value_vec) {
        list_value.add_values()->CopyFrom(Rcpp::as<google::protobuf::Value>(value));
      }
      out.push_back(list_value);
    }
  }
  return out;
}

template <>
inline std::vector<tl::optional<tensorboard::hparams::Interval>>
Rcpp::as<std::vector<tl::optional<tensorboard::hparams::Interval>>> (SEXP x) {
  std::vector<tl::optional<tensorboard::hparams::Interval>> out;
  auto r_intervals = Rcpp::as<Rcpp::List>(x);

  auto r_min_value = Rcpp::as<Rcpp::NumericVector>(r_intervals["min_value"]);
  auto r_max_value = Rcpp::as<Rcpp::NumericVector>(r_intervals["max_value"]);
  for (R_xlen_t i = 0; i < r_min_value.size(); i++) {
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
inline std::vector<tensorboard::hparams::HParamInfo> Rcpp::as<std::vector<tensorboard::hparams::HParamInfo>>(SEXP x) {
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
inline std::vector<tensorboard::hparams::MetricName>
Rcpp::as<std::vector<tensorboard::hparams::MetricName>> (SEXP x) {
  auto r_metric_name = Rcpp::as<Rcpp::List>(x);

  auto r_tag = Rcpp::as<Rcpp::CharacterVector>(r_metric_name["tag"]);
  auto r_group = Rcpp::as<Rcpp::CharacterVector>(r_metric_name["group"]);

  std::vector<tensorboard::hparams::MetricName> out;
  for (R_xlen_t i = 0; i < r_tag.size(); i++) {
    tensorboard::hparams::MetricName metric_name;
    metric_name.set_tag(r_tag[i]);
    if (!Rcpp::CharacterVector::is_na(r_group[i])) {
      metric_name.set_group(r_group[i]);
    }
    out.push_back(metric_name);
  }
  return out;
}

template <>
inline std::vector<tensorboard::hparams::DatasetType>
Rcpp::as<std::vector<tensorboard::hparams::DatasetType>> (SEXP x) {
  auto r_dataset_type = Rcpp::as<std::vector<std::string>>(x);
  std::vector<tensorboard::hparams::DatasetType> out;
  for (auto r_type : r_dataset_type) {
    if (r_type == "training") {
      out.push_back(tensorboard::hparams::DatasetType::DATASET_TRAINING);
    } else if (r_type == "validation") {
      out.push_back(tensorboard::hparams::DatasetType::DATASET_VALIDATION);
    } else if (r_type == "unknown") {
      out.push_back(tensorboard::hparams::DatasetType::DATASET_UNKNOWN);
    } else {
      Rcpp::stop("Unknown dataset type: %s", r_type);
    }
  }
  return out;
}

template <>
inline std::vector<tensorboard::hparams::MetricInfo>
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
inline std::vector<tl::optional<tensorboard::hparams::Experiment>>
Rcpp::as<std::vector<tl::optional<tensorboard::hparams::Experiment>>> (SEXP x) {
  auto r_experiments = Rcpp::as<Rcpp::List>(x);

  auto r_name = Rcpp::as<std::vector<std::string>>(r_experiments["name"]);
  auto r_description = Rcpp::as<std::vector<std::string>>(r_experiments["description"]);
  auto r_user = Rcpp::as<std::vector<std::string>>(r_experiments["user"]);
  auto r_time_created_secs = Rcpp::as<std::vector<int64_t>>(r_experiments["time_created_secs"]);
  auto r_hparam_infos = Rcpp::as<Rcpp::List>(r_experiments["hparam_infos"]);
  auto r_metric_infos = Rcpp::as<Rcpp::List>(r_experiments["metric_infos"]);

  std::vector<tl::optional<tensorboard::hparams::Experiment>> out;
  for (size_t i = 0; i < r_name.size(); i++) {
    if (r_is_na(r_hparam_infos[i])) {
      out.push_back(tl::nullopt);
    } else {
      tensorboard::hparams::Experiment experiment;

      experiment.set_name(r_name[i]);
      experiment.set_description(r_description[i]);
      experiment.set_user(r_user[i]);
      experiment.set_time_created_secs(r_time_created_secs[i]);

      auto hparam_infos = Rcpp::as<std::vector<tensorboard::hparams::HParamInfo>>(r_hparam_infos[i]);
      for (size_t j = 0; j < hparam_infos.size(); j++) {
        experiment.add_hparam_infos()->CopyFrom(hparam_infos[j]);
      }

      auto metric_infos = Rcpp::as<std::vector<tensorboard::hparams::MetricInfo>>(r_metric_infos[i]);
      for (size_t j = 0; j < metric_infos.size(); j++) {
        experiment.add_metric_infos()->CopyFrom(metric_infos[j]);
      }

      out.push_back(experiment);
    }
  }

  return out;
}

template<>
inline google::protobuf::Map<std::string, google::protobuf::Value>
Rcpp::as<google::protobuf::Map<std::string, google::protobuf::Value>> (SEXP x) {
  auto r_list = Rcpp::as<Rcpp::List>(x);
  auto r_name = Rcpp::as<std::vector<std::string>>(r_list.names());

  google::protobuf::Map<std::string, google::protobuf::Value> out;
  for (R_xlen_t i=0; i< r_list.size(); i++) {
    out.insert(google::protobuf::MapPair<std::string,google::protobuf::Value>(
        r_name[i],
              Rcpp::as<google::protobuf::Value>(r_list[i])
    ));
  }
  return out;
}

template<>
inline std::vector<tl::optional<google::protobuf::Map<std::string, google::protobuf::Value>>>
Rcpp::as<std::vector<tl::optional<google::protobuf::Map<std::string, google::protobuf::Value>>>> (SEXP x) {
  auto r_list = Rcpp::as<Rcpp::List>(x);
  std::vector<tl::optional<google::protobuf::Map<std::string, google::protobuf::Value>>> out;
  for (R_xlen_t i = 0; i < r_list.size(); i++) {
    if (r_is_na(r_list[i])) {
      out.push_back(tl::nullopt);
    } else {
      out.push_back(Rcpp::as<google::protobuf::Map<std::string, google::protobuf::Value>>(r_list[i]));
    }
  }
  return out;
}

template<>
inline std::vector<tl::optional<tensorboard::hparams::SessionStartInfo>>
Rcpp::as<std::vector<tl::optional<tensorboard::hparams::SessionStartInfo>>> (SEXP x) {
  auto r_session_start = Rcpp::as<Rcpp::List>(x);

  auto r_hparams = Rcpp::as<std::vector<tl::optional<google::protobuf::Map<std::string, google::protobuf::Value>>>>(r_session_start["hparams"]);
  auto r_model_uri = Rcpp::as<std::vector<std::string>>(r_session_start["model_uri"]);
  auto r_monitor_url = Rcpp::as<std::vector<std::string>>(r_session_start["monitor_url"]);
  auto r_group_name = Rcpp::as<std::vector<std::string>>(r_session_start["group_name"]);
  auto r_start_time_secs = Rcpp::as<std::vector<std::int64_t>>(r_session_start["start_time_secs"]);

  std::vector<tl::optional<tensorboard::hparams::SessionStartInfo>> out;
  for (size_t i = 0; i< r_hparams.size(); i++) {
    if (!r_hparams[i].has_value()) {
      out.push_back(tl::nullopt);
    } else {
      tensorboard::hparams::SessionStartInfo info;
      auto hparams = r_hparams[i].value();

      info.mutable_hparams()->insert(hparams.begin(), hparams.end());
      info.set_model_uri(r_model_uri[i]);
      info.set_monitor_url(r_monitor_url[i]);
      info.set_group_name(r_group_name[i]);
      info.set_start_time_secs(r_start_time_secs[i]);
      out.push_back(info);
    }
  }
  return out;
}

template <>
inline tensorboard::hparams::HParamsPluginData
Rcpp::as<tensorboard::hparams::HParamsPluginData> (SEXP x) {
  auto r_plugin_data = Rcpp::as<Rcpp::List>(x);

  auto r_version = Rcpp::as<std::vector<std::int64_t>>(r_plugin_data["version"]);

  auto r_experiment = Rcpp::as<std::vector<tl::optional<tensorboard::hparams::Experiment>>>(r_plugin_data["experiment"]);
  auto r_session_start_info = Rcpp::as<std::vector<tl::optional<tensorboard::hparams::SessionStartInfo>>>(r_plugin_data["session_start_info"]);

  tensorboard::hparams::HParamsPluginData plugin_data;
  plugin_data.set_version(r_version[0]);

  if (r_experiment[0].has_value()) {
    plugin_data.mutable_experiment()->CopyFrom(r_experiment[0].value());
  }

  if (r_session_start_info[0].has_value()) {
    plugin_data.mutable_session_start_info()->CopyFrom(r_session_start_info[0].value());
  }

  return plugin_data;
}
