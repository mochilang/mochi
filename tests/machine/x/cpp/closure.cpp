#include <iostream>

auto makeAdder(int n) {
  return [=](int x) { return (x + n); };
}

int main() {
  auto add10 = makeAdder(10);
  std::cout << std::boolalpha << add10(7) << std::endl;
  return 0;
}
