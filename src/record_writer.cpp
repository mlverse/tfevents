#include "record_writer.h"
#include "crc.h"

// helper functions

// some changes from:
// https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/lib/core/coding.cc#L43
void encode_fixed_64(char* buf, std::uint64_t value) {
  memcpy(buf, &value, sizeof(value));
}

void encode_fixed_32(char* buf, std::uint32_t value) {
  memcpy(buf, &value, sizeof(value));
}

// found implementation here: https://bidetly.io/2017/02/08/crc-part-1/
std::uint32_t crc32_boost(const char* first, const char* last) {
  boost::crc_optimal<32, 0x1edc6f41, 0xffffffff, 0xffffffff, true, true> machine;
  machine.process_block(first, last);
  return machine.checksum();
}

// mask delta constant
// https://github.com/tensorflow/tensorflow/blob/754048a0453a04a761e112ae5d99c149eb9910dd/tensorflow/core/lib/hash/crc32c.h#L33
const uint32_t mask_delta = 0xa282ead8ul;

// making crc
// https://github.com/tensorflow/tensorflow/blob/754048a0453a04a761e112ae5d99c149eb9910dd/tensorflow/core/lib/hash/crc32c.h#L40
uint32_t mask(uint32_t crc) {
  return ((crc >> 15) | (crc << 17)) + mask_delta;
};

std::uint32_t masked_crc (char * data, std::size_t n) {
  return mask(crc32_boost(data, data + n));
}

RecordWriter::RecordWriter (std::string path) {
  this->path = path;
  this->writer.open(path);
}

RecordWriter::~RecordWriter () {
  this->writer.close();
}

bool RecordWriter::write_record (std::string data) {

  // Format of a single record:
  //  uint64    length
  //  uint32    masked crc of length
  //  byte      data[length]
  //  uint32    masked crc of data

  char length[sizeof(std::uint64_t)];
  encode_fixed_64(length, data.size());

  char length_crc[sizeof(std::uint32_t)];
  encode_fixed_32(length_crc, masked_crc(length, sizeof(std::uint64_t)));

  char data_crc[sizeof(std::uint32_t)];
  char * data_array = const_cast<char*>(data.c_str());
  encode_fixed_32(data_crc, masked_crc(data_array, data.size()));

  this->writer.write(length, sizeof(length));
  this->writer.write(length_crc, sizeof(length_crc));
  this->writer.write(data.c_str(), data.length());
  this->writer.write(data_crc, sizeof(data_crc));

  return true;
}

void RecordWriter::flush () {
  this->writer.flush();
}

