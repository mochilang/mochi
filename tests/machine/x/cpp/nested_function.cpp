#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto outer(auto x) {
  auto inner = [=](int y) { return (x + y); };
  return inner(5);
}

int main() {
  {
    std::cout << std::boolalpha << outer(3);
    std::cout << std::endl;
  }
  return 0;
}
