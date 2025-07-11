#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  std::vector<int> xs = std::vector<int>{1, 2, 3};
  std::cout << std::boolalpha
            << (std::find(xs.begin(), xs.end(), 2) != xs.end()) << std::endl;
  std::cout << std::boolalpha
            << (!((std::find(xs.begin(), xs.end(), 5) != xs.end())))
            << std::endl;
  return 0;
}
