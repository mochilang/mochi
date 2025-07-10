#include <iostream>
#include <vector>

int main() {
  std::cout << std::boolalpha << ([&](auto v) {
    int s = 0;
    for (auto x : v)
      s += x;
    return v.empty() ? 0 : (double)s / v.size();
  })(std::vector<decltype(1)>{1, 2, 3})
            << std::endl;
  return 0;
}
