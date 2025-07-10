#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T> void __json(const T &);
inline void __json(int v) { std::cout << v; }
inline void __json(double v) { std::cout << v; }
inline void __json(bool v) { std::cout << (v ? "true" : "false"); }
inline void __json(const std::string &v) { std::cout << "\"" << v << "\""; }
inline void __json(const char *v) { std::cout << "\"" << v << "\""; }
template <typename T> void __json(const std::vector<T> &v) {
  std::cout << "[";
  bool first = true;
  for (const auto &x : v) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(x);
  }
  std::cout << "]";
}
template <typename K, typename V> void __json(const std::map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}
template <typename K, typename V>
void __json(const std::unordered_map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}

auto twoSum(auto nums, auto target) {
  auto n = nums.size();
  for (int i = 0; i < n; ++i) {
    for (int j = (i + 1); j < n; ++j) {
      if (((nums[i] + nums[j]) == target)) {
        return std::vector<decltype(i)>{i, j};
      }
    }
  }
  return std::vector<decltype((-1))>{(-1), (-1)};
}

int main() {
  auto result = twoSum(std::vector<decltype(2)>{2, 7, 11, 15}, 9);
  {
    std::cout << std::boolalpha << result[0];
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << result[1];
    std::cout << std::endl;
  }
  return 0;
}
