#include <iostream>
#include <string>
#include <unordered_map>

int main() {
  auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                        {std::string("b"), 2}};
  std::cout << std::boolalpha << m[std::string("b")] << std::endl;
  return 0;
}
