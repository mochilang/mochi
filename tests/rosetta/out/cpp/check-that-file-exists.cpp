// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:13Z
#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

auto printStat(auto fs, std::string path) {
  if ((std::find(fs.begin(), fs.end(), path) != fs.end())) {
    if (fs[path]) {
      std::cout << (path + std::string(" is a directory")) << std::endl;
    } else {
      std::cout << (path + std::string(" is a file")) << std::endl;
    }
  } else {
    std::cout << ((std::string("stat ") + path) +
                  std::string(": no such file or directory"))
              << std::endl;
  }
}

auto main() {
  auto fs = std::unordered_map<int, int>{};
  fs[std::string("docs")] = true;
  for (auto p : std::vector<std::string>{
           std::string("input.txt"), std::string("/input.txt"),
           std::string("docs"), std::string("/docs")}) {
    printStat(fs, p);
  }
}

int main() {
  main();
  return 0;
}
