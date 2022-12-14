#include <Rcpp.h>
#include "generated/summary.pb.h"

class EventFileIterator {
public:
  std::ifstream file;
  std::uint64_t current_pos = 0;
  std::string path;
  EventFileIterator (const std::string& path);
  tensorboard::Event get_next ();
};

