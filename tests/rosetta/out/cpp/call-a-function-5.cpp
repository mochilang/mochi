// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:11Z
#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>

int doIt(auto p) {
  auto b = 0;
  if ((std::find(p.begin(), p.end(), std::string("b")) != p.end())) {
    b = p[std::string("b")];
  }
  return ((p[std::string("a")] + b) + p[std::string("c")]);
}

auto main() {
  auto p = std::unordered_map<int, int>{};
  p[std::string("a")] = 1;
  p[std::string("c")] = 9;
  std::cout << std::to_string(doIt(p)) << std::endl;
}

int main() {
  main();
  return 0;
}
