// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:11Z
#include <iostream>
#include <string>

std::string strdup(std::string s) { return (s + std::string("")); }

auto main() {
  auto go1 = std::string("hello C");
  auto c2 = strdup(go1);
  std::cout << c2 << std::endl;
}

int main() {
  main();
  return 0;
}
