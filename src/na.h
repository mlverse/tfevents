#pragma once

static bool r_is_na (SEXP x) {
  static auto pkg = Rcpp::Environment::namespace_env("tfevents");
  auto isna = Rcpp::Function(pkg["is_na"]);
  return Rcpp::as<bool>(isna(x));
}
