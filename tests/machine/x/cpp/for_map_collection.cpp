#include <iostream>
#include <string>
#include <unordered_map>

int main() {
  auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                        {std::string("b"), 2}};
  for (const auto &__tmp1 : m) {
    auto k = __tmp1.first;
    std::cout << std::boolalpha << k << std::endl;
  }
  return 0;
}
