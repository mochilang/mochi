#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha
              << (std::vector<decltype(1)>{1, 2} union std::vector<decltype(2)>{
                     2, 3});
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::vector<decltype(1)>{
                     1, 2, 3} except std::vector<decltype(2)>{2});
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::vector<decltype(1)>{
                     1, 2, 3} intersect std::vector<decltype(2)>{2, 4});
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::vector<decltype(1)>{1, 2} union std::vector<decltype(2)>{
                      2, 3})
                     .size();
    std::cout << std::endl;
  }
  return 0;
}
