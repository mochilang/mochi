#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  std::vector<int> data = std::vector<int>{1, 2};
  auto flag =
      std::any_of(data.begin(), data.end(), [&](auto x) { return (x == 1); });
  std::cout << std::boolalpha << flag << std::endl;
  return 0;
}
