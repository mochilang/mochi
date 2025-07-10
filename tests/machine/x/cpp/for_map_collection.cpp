#include <iostream>
#include <unordered_map>

auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                      {std::string("b"), 2}};

int main() {
  for (auto k : m) {
    std::cout << std::boolalpha << k.first << ' ' << k.second << std::endl;
  }
  return 0;
}
