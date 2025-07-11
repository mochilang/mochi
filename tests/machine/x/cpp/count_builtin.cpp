#include <iostream>
#include <vector>

int main() {
  std::cout << std::boolalpha << ((int)std::vector<int>{1, 2, 3}.size())
            << std::endl;
  return 0;
}
