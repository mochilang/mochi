#include <iostream>
#include <unordered_map>

auto m = std::unordered_map<int, std::string>{{1, std::string("a")},
                                              {2, std::string("b")}};

int main() {
  std::cout << std::boolalpha << (m.count(1) > 0) << std::endl;
  std::cout << std::boolalpha << (m.count(3) > 0) << std::endl;
  return 0;
}
