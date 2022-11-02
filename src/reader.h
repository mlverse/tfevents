#include <Rcpp.h>

class EventFileIterator {
public:
  std::ifstream file;
  EventFileIterator (const std::string& path);
  tensorboard::Event get_next ();
};

namespace tfevents {

class Event {
public:
  tensorboard::Event event;
  Event (const tensorboard::Event& e);
  operator SEXP () const;
};

}
