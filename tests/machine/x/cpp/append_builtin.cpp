#include <iostream>
#include <vector>

template <typename T, typename U>
std::vector<T> __append(const std::vector<T> &v, const U &x) {
  auto r = v;
  r.push_back(x);
  return r;
}
int main() {
  std::vector<int> a = std::vector<int>{1, 2};
  {
    auto __tmp1 = __append(a, 3);
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
