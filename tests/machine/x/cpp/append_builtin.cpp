#include <iostream>
#include <vector>

std::vector<int> a = std::vector<decltype(1)>{1, 2};

int main() {
  {
    auto __tmp1 = ([&](auto v) {
      v.push_back(3);
      return v;
    })(a);
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  return 0;
}
