#include <iostream>
#include <vector>

int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2, 3};
  auto result = ([&]() {
    int __sum = 0;
    for (auto n : nums) {
      if (!((n > 1)))
        continue;
      __sum += n;
    }
    return __sum;
  })();
  std::cout << result << std::endl;
  return 0;
}
