#include <iostream>
#include <unordered_map>

auto scores =
    std::unordered_map<std::string, decltype(1)>{{std::string("alice"), 1}};

int main() {
  scores[std::string("bob")] = 2;
  {
    std::cout << std::boolalpha << scores[std::string("bob")];
    std::cout << std::endl;
  }
  return 0;
}
