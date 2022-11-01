#include <fstream>

class RecordWriter {
public:
  std::string path;
  std::ofstream writer;
  RecordWriter (std::string path);
  ~RecordWriter ();
  bool write_record (std::string data);
  void flush();
};
