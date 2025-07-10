#include <iostream>
#include <vector>

auto matrix = std::vector<decltype(std::vector<decltype(1)>{1, 2})>{
    std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}};

int main() {
  matrix[1][0] = 5;
  std::cout << std::boolalpha << matrix[1][0] << std::endl;
  return 0;
}
