#include <iostream>
#include <vector>

template <typename T> double __avg(const std::vector<T> &v) {
  if (v.empty())
    return 0;
  double s = 0;
  for (const auto &x : v)
    s += x;
  return s / v.size();
}
int main() {
  std::cout << std::boolalpha << __avg(std::vector<decltype(1)>{1, 2, 3})
            << std::endl;
  return 0;
}
