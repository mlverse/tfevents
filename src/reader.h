#pragma once

#include <Rcpp.h>
#include "generated/summary.pb.h"

class EventFileIterator {
public:
  std::ifstream file;
  std::uint64_t current_pos = 0;
  std::string path;
  std::string run_name;
  EventFileIterator (const std::string& path, const std::string& run_name);
  tensorboard::Event get_next ();
};

