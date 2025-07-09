#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto matrix = std::vector<decltype(std::vector<decltype(1)>{1, 2})>{
      std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}};
  matrix[1][0] = 5;
  {
    std::cout << std::boolalpha << matrix[1][0];
    std::cout << std::endl;
  }
  return 0;
}
