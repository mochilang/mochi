#include <iostream>
#include <unordered_map>

int main() {
  std::cout
      << std::boolalpha
      << std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                      {std::string("b"), 2}}
             .size()
      << std::endl;
  return 0;
}
