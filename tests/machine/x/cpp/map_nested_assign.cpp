#include <iostream>
#include <string>
#include <unordered_map>

int main() {
  auto data = std::unordered_map<std::string,
                                 std::unordered_map<std::string, decltype(1)>>{
      {std::string("outer"), std::unordered_map<std::string, decltype(1)>{
                                 {std::string("inner"), 1}}}};
  data[std::string("outer")][std::string("inner")] = 2;
  std::cout << std::boolalpha
            << data[std::string("outer")][std::string("inner")] << std::endl;
  return 0;
}
