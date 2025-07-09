#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    auto __tmp1 = std::vector<decltype(1)>{1, 2, 3}[1];
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = std::vector<decltype(1)>{1, 2, 3}[0];
    for (size_t i = 0; i < __tmp2.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp2[i];
    }
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << std::string("hello")[1];
    std::cout << std::endl;
  }
  return 0;
}
