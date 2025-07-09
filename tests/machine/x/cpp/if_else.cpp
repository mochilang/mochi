#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto x = 5;
  if ((x > 3)) {
    {
      std::cout << std::boolalpha << std::string("big");
      std::cout << std::endl;
    }
  } else {
    {
      std::cout << std::boolalpha << std::string("small");
      std::cout << std::endl;
    }
  }
  return 0;
}
