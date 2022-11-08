static bool r_is_na (SEXP x) {
  static auto pkg = Rcpp::Environment::namespace_env("base");
  auto isna = Rcpp::Function(pkg["is.na"]);
  return isna(x);
}
