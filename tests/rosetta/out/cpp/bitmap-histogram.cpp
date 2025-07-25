// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:10Z
#include <iostream>
#include <string>
#include <vector>

auto image() {
  return std::vector<decltype(std::vector<int>{0, 0, 10000})>{
      std::vector<int>{0, 0, 10000}, std::vector<int>{65535, 65535, 65535},
      std::vector<int>{65535, 65535, 65535}};
}

auto histogram(auto g, int bins) {
  if ((bins <= 0)) {
    bins = g[0].size();
  }
  auto h = std::vector<int>{};
  auto i = 0;
  while ((i < bins)) {
    h.push_back(0);
    i = (i + 1);
  }
  auto y = 0;
  while ((y < g.size())) {
    auto row = g[y];
    auto x = 0;
    while ((x < row.size())) {
      auto p = row[x];
      auto idx = std::stoi(((((p * ((bins - 1)))) / 65535)));
      h[idx] = (h[idx] + 1);
      x = (x + 1);
    }
    y = (y + 1);
  }
  return h;
}

int medianThreshold(auto h) {
  auto lb = 0;
  auto ub = (h.size() - 1);
  auto lSum = 0;
  auto uSum = 0;
  while ((lb <= ub)) {
    if (((lSum + h[lb]) < (uSum + h[ub]))) {
      lSum = (lSum + h[lb]);
      lb = (lb + 1);
    } else {
      uSum = (uSum + h[ub]);
      ub = (ub - 1);
    }
  }
  return std::stoi(((((ub * 65535)) / h.size())));
}

auto threshold(auto g, int t) {
  auto out = std::vector<int>{};
  auto y = 0;
  while ((y < g.size())) {
    auto row = g[y];
    auto newRow = std::vector<int>{};
    auto x = 0;
    while ((x < row.size())) {
      if ((row[x] < t)) {
        newRow.push_back(0);
      } else {
        newRow.push_back(65535);
      }
      x = (x + 1);
    }
    out.push_back(newRow);
    y = (y + 1);
  }
  return out;
}

auto printImage(auto g) {
  auto y = 0;
  while ((y < g.size())) {
    auto row = g[y];
    auto line = std::string("");
    auto x = 0;
    while ((x < row.size())) {
      if ((row[x] == 0)) {
        line = (line + std::string("0"));
      } else {
        line = (line + std::string("1"));
      }
      x = (x + 1);
    }
    std::cout << line << std::endl;
    y = (y + 1);
  }
}

auto main() {
  auto img = image();
  auto h = histogram(img, 0);
  std::cout << (std::string("Histogram: ") + std::to_string(h)) << std::endl;
  auto t = medianThreshold(h);
  std::cout << (std::string("Threshold: ") + std::to_string(t)) << std::endl;
  auto bw = threshold(img, t);
  printImage(bw);
}

int main() {
  main();
  return 0;
}
