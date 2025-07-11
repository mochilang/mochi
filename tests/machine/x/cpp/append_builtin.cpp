#include <iostream>
#include <vector>

std::vector<int> a = std::vector<decltype(1)>{1, 2};

int main() {
  {
    auto __tmp1 = ([&](auto v) {
      v.push_back(3);
      return v;
    })(a);
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  return 0;
}
