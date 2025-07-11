#include <iostream>
#include <string>
#include <unordered_map>

int main() {
  auto scores =
      std::unordered_map<std::string, decltype(1)>{{std::string("alice"), 1}};
  scores[std::string("bob")] = 2;
  std::cout << std::boolalpha << scores[std::string("bob")] << std::endl;
  return 0;
}
