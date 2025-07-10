#include <iostream>
#include <unordered_map>

auto x = 3;
auto y = 4;
auto m = std::unordered_map<std::string, decltype(x)>{{std::string("a"), x},
                                                      {std::string("b"), y}};

int main() {
  {
    std::cout << std::boolalpha << m[std::string("a")];
    std::cout << ' ';
    std::cout << std::boolalpha << m[std::string("b")];
    std::cout << std::endl;
  }
  return 0;
}
