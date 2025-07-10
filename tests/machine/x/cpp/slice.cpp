#include <iostream>
#include <vector>

int main() {
  {
    auto __tmp1 = ([&](auto v) {
      return std::vector<int>(v.begin() + 1, v.begin() + 3);
    })(std::vector<decltype(1)>{1, 2, 3});
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = ([&](auto v) {
      return std::vector<int>(v.begin() + 0, v.begin() + 2);
    })(std::vector<decltype(1)>{1, 2, 3});
    for (size_t i = 0; i < __tmp2.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp2[i];
    }
    std::cout << std::endl;
  }
  std::cout << std::boolalpha
            << std::string(std::string("hello")).substr(1, (4) - (1))
            << std::endl;
  return 0;
}
