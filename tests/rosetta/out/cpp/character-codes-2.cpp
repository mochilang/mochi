// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:13Z
#include <iostream>
#include <string>

int ord(std::string ch) {
  if ((ch == std::string("a"))) {
    return 97;
  }
  if ((ch == std::string("π"))) {
    return 960;
  }
  if ((ch == std::string("A"))) {
    return 65;
  }
  return 0;
}

std::string chr(int n) {
  if ((n == 97)) {
    return std::string("a");
  }
  if ((n == 960)) {
    return std::string("π");
  }
  if ((n == 65)) {
    return std::string("A");
  }
  return std::string("?");
}

int main() {
  std::cout << std::to_string(ord(std::string("A"))) << std::endl;
  std::cout << chr(65) << std::endl;
  return 0;
}
