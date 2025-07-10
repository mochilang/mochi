#include <iostream>
#include <unordered_map>

auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                      {std::string("b"), 2}};

int main() {
  std::cout << std::boolalpha << m[std::string("b")] << std::endl;
  return 0;
}
