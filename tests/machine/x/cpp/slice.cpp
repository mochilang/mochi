#include <iostream>
#include <string>
#include <vector>

int main() {
  {
    auto __tmp1 = ([&](auto v) {
      return std::vector<decltype(std::vector<int>{1, 2, 3}[0])>(v.begin() + 1,
                                                                 v.begin() + 3);
    })(std::vector<int>{1, 2, 3});
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = ([&](auto v) {
      return std::vector<decltype(std::vector<int>{1, 2, 3}[0])>(v.begin() + 0,
                                                                 v.begin() + 2);
    })(std::vector<int>{1, 2, 3});
    bool first = true;
    for (const auto &_x : __tmp2) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  std::cout << std::string(std::string("hello")).substr(1, (4) - (1))
            << std::endl;
  return 0;
}
