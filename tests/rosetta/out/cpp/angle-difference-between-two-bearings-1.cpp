// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:06Z
#include <iostream>
#include <vector>

float angleDiff(float b1, float b2) {
  auto d = (b2 - b1);
  if ((d < -180)) {
    return (d + 360);
  }
  if ((d > 180)) {
    return (d - 360);
  }
  return d;
}

int main() {
  auto testCases = std::vector<decltype(std::vector<int>{20, 45})>{
      std::vector<int>{20, 45},
      std::vector<int>{-45, 45},
      std::vector<int>{-85, 90},
      std::vector<int>{-95, 90},
      std::vector<int>{-45, 125},
      std::vector<int>{-45, 145},
      std::vector<double>{29.4803, -88.6381},
      std::vector<double>{-78.3251, -159.036}};
  for (auto tc : testCases) {
    std::cout << angleDiff(tc[0], tc[1]) << std::endl;
  }
  return 0;
}
