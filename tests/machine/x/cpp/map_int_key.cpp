#include <iostream>
#include <unordered_map>

int main() {
  auto m = std::unordered_map<int, std::string>{{1, std::string("a")},
                                                {2, std::string("b")}};
  {
    std::cout << std::boolalpha << m[1];
    std::cout << std::endl;
  }
  return 0;
}
