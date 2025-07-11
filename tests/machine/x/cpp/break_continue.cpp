#include <iostream>
#include <string>
#include <vector>

int main() {
  auto numbers = std::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9};
  for (auto n : numbers) {
    if (((n % 2) == 0)) {
      continue;
    }
    if ((n > 7)) {
      break;
    }
    {
      std::cout << std::string("odd number:");
      std::cout << ' ';
      std::cout << std::boolalpha << n;
      std::cout << std::endl;
    }
  }
  return 0;
}
