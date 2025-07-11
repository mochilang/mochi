#include <iostream>
#include <vector>

int main() {
  auto matrix = std::vector<decltype(std::vector<int>{1, 2})>{
      std::vector<int>{1, 2}, std::vector<int>{3, 4}};
  matrix[1][0] = 5;
  std::cout << std::boolalpha << matrix[1][0] << std::endl;
  return 0;
}
