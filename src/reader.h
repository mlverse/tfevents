#include <Rcpp.h>
#include "generated/summary.pb.h"

class EventFileIterator {
public:
  std::ifstream file;
  EventFileIterator (const std::string& path);
  tensorboard::Event get_next ();
};

