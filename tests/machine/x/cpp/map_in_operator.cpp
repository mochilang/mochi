#include <iostream>
#include <string>
#include <unordered_map>

int main() {
  auto m = std::unordered_map<int, std::string>{{1, std::string("a")},
                                                {2, std::string("b")}};
  std::cout << ((m.count(1) > 0) ? "true" : "false") << std::endl;
  std::cout << ((m.count(3) > 0) ? "true" : "false") << std::endl;
  return 0;
}
