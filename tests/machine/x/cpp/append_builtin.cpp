#include <iostream>
#include <vector>

int main() {
  std::vector<int> a = std::vector<int>{1, 2};
  a.push_back(3);
  {
    auto __tmp1 = a;
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
