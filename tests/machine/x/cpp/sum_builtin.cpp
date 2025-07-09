#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha << ([&](auto v) {
      return std::accumulate(v.begin(), v.end(), 0);
    })(std::vector<decltype(1)>{1, 2, 3});
    std::cout << std::endl;
  }
  return 0;
}
