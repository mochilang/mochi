#include <iostream>
#include <unordered_map>

int main() {
  auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                        {std::string("b"), 2}};
  {
    std::cout << std::boolalpha << (m.count(std::string("a")) > 0);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (m.count(std::string("c")) > 0);
    std::cout << std::endl;
  }
  return 0;
}
