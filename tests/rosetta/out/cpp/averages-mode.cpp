// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:08Z
#include <algorithm>
#include <iostream>
#include <unordered_map>
#include <vector>

int main() {
  std::vector<int> arr1 = {2, 7, 1, 8, 2};
  auto counts1 = std::unordered_map<int, int>{};
  auto keys1 = std::vector<int>{};
  auto i = 0;
  while ((i < arr1.size())) {
    auto v = arr1[i];
    if ((std::find(counts1.begin(), counts1.end(), v) != counts1.end())) {
      counts1[v] = (counts1[v] + 1);
    } else {
      counts1[v] = 1;
      keys1.push_back(v);
    }
    i = (i + 1);
  }
  auto max1 = 0;
  i = 0;
  while ((i < keys1.size())) {
    auto k = keys1[i];
    auto c = counts1[k];
    if ((c > max1)) {
      max1 = c;
    }
    i = (i + 1);
  }
  auto modes1 = std::vector<int>{};
  i = 0;
  while ((i < keys1.size())) {
    auto k = keys1[i];
    if ((counts1[k] == max1)) {
      modes1.push_back(k);
    }
    i = (i + 1);
  }
  std::cout << std::to_string(modes1) << std::endl;
  std::vector<int> arr2 = {2, 7, 1, 8, 2, 8};
  auto counts2 = std::unordered_map<int, int>{};
  auto keys2 = std::vector<int>{};
  i = 0;
  while ((i < arr2.size())) {
    auto v = arr2[i];
    if ((std::find(counts2.begin(), counts2.end(), v) != counts2.end())) {
      counts2[v] = (counts2[v] + 1);
    } else {
      counts2[v] = 1;
      keys2.push_back(v);
    }
    i = (i + 1);
  }
  auto max2 = 0;
  i = 0;
  while ((i < keys2.size())) {
    auto k = keys2[i];
    auto c = counts2[k];
    if ((c > max2)) {
      max2 = c;
    }
    i = (i + 1);
  }
  auto modes2 = std::vector<int>{};
  i = 0;
  while ((i < keys2.size())) {
    auto k = keys2[i];
    if ((counts2[k] == max2)) {
      modes2.push_back(k);
    }
    i = (i + 1);
  }
  std::cout << std::to_string(modes2) << std::endl;
  return 0;
}
