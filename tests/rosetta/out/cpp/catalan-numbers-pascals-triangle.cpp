// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:13Z
#include <iostream>
#include <string>
#include <vector>

int main() {
  auto n = 15;
  auto t = std::vector<int>{};
  for (int _ = 0; _ < ((n + 2)); _++) {
    t.push_back(0);
  }
  t[1] = 1;
  for (int i = 1; i < ((n + 1)); i++) {
    auto j = i;
    while ((j > 1)) {
      t[j] = (t[j] + t[(j - 1)]);
      j = (j - 1);
    }
    t[(i + 1)] = t[i];
    j = (i + 1);
    while ((j > 1)) {
      t[j] = (t[j] + t[(j - 1)]);
      j = (j - 1);
    }
    auto cat = (t[(i + 1)] - t[i]);
    if ((i < 10)) {
      std::cout << (((std::string(" ") + std::to_string(i)) +
                     std::string(" : ")) +
                    std::to_string(cat))
                << std::endl;
    } else {
      std::cout << ((std::to_string(i) + std::string(" : ")) +
                    std::to_string(cat))
                << std::endl;
    }
  }
  return 0;
}
