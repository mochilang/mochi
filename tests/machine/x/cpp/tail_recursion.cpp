#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto sum_rec(auto n, auto acc) {
  if ((n == 0)) {
    return acc;
  }
  return sum_rec((n - 1), (acc + n));
}

int main() {
  {
    std::cout << std::boolalpha << sum_rec(10, 0);
    std::cout << std::endl;
  }
  return 0;
}
