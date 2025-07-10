#include <iostream>
#include <vector>

int main() {
  {
    auto __tmp1 = std::vector<decltype(1)>{1, 2, 3}.size();
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  return 0;
}
