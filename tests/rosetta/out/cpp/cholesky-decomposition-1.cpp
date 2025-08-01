// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:14Z
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

float sqrtApprox(float x) {
  auto guess = x;
  auto i = 0;
  while ((i < 20)) {
    guess = (((guess + (x / guess))) / 2);
    i = (i + 1);
  }
  return guess;
}

auto makeSym(int order, auto elements) {
  return std::unordered_map<std::string, decltype(order)>{
      {std::string("order"), order}, {std::string("ele"), elements}};
}

auto unpackSym(auto m) {
  auto n = m[std::string("order")];
  auto ele = m[std::string("ele")];
  auto mat = std::vector<int>{};
  auto idx = 0;
  auto r = 0;
  while ((r < n)) {
    auto row = std::vector<int>{};
    auto c = 0;
    while ((c <= r)) {
      row.push_back(ele[idx]);
      idx = (idx + 1);
      c = (c + 1);
    }
    while ((c < n)) {
      row.push_back(0);
      c = (c + 1);
    }
    mat.push_back(row);
    r = (r + 1);
  }
  r = 0;
  while ((r < n)) {
    auto c = (r + 1);
    while ((c < n)) {
      mat[r][c] = mat[c][r];
      c = (c + 1);
    }
    r = (r + 1);
  }
  return mat;
}

auto printMat(auto m) {
  auto i = 0;
  while ((i < m.size())) {
    auto line = std::string("");
    auto j = 0;
    while ((j < m[i].size())) {
      line = (line + std::to_string(m[i][j]));
      if ((j < (m[i].size() - 1))) {
        line = (line + std::string(" "));
      }
      j = (j + 1);
    }
    std::cout << line << std::endl;
    i = (i + 1);
  }
}

auto printSym(auto m) { printMat(unpackSym(m)); }

auto printLower(auto m) {
  auto n = m[std::string("order")];
  auto ele = m[std::string("ele")];
  auto mat = std::vector<int>{};
  auto idx = 0;
  auto r = 0;
  while ((r < n)) {
    auto row = std::vector<int>{};
    auto c = 0;
    while ((c <= r)) {
      row.push_back(ele[idx]);
      idx = (idx + 1);
      c = (c + 1);
    }
    while ((c < n)) {
      row.push_back(0);
      c = (c + 1);
    }
    mat.push_back(row);
    r = (r + 1);
  }
  printMat(mat);
}

auto choleskyLower(auto a) {
  auto n = a[std::string("order")];
  auto ae = a[std::string("ele")];
  auto le = std::vector<int>{};
  auto idx = 0;
  while ((idx < ae.size())) {
    le.push_back(0);
    idx = (idx + 1);
  }
  auto row = 1;
  auto col = 1;
  auto dr = 0;
  auto dc = 0;
  auto i = 0;
  while ((i < ae.size())) {
    auto e = ae[i];
    if ((i < dr)) {
      auto d = (((e - le[i])) / le[dc]);
      le[i] = d;
      auto ci = col;
      auto cx = dc;
      auto j = (i + 1);
      while ((j <= dr)) {
        cx = (cx + ci);
        ci = (ci + 1);
        le[j] = (le[j] + (d * le[cx]));
        j = (j + 1);
      }
      col = (col + 1);
      dc = (dc + col);
    } else {
      le[i] = sqrtApprox((e - le[i]));
      row = (row + 1);
      dr = (dr + row);
      col = 1;
      dc = 0;
    }
    i = (i + 1);
  }
  return std::unordered_map<std::string, decltype(n)>{{std::string("order"), n},
                                                      {std::string("ele"), le}};
}

auto demo(auto a) {
  std::cout << std::string("A:") << std::endl;
  printSym(a);
  std::cout << std::string("L:") << std::endl;
  auto l = choleskyLower(a);
  printLower(l);
}

int main() {
  demo(makeSym(3, std::vector<int>{25, 15, 18, -5, 0, 11}));
  demo(makeSym(4, std::vector<int>{18, 22, 70, 54, 86, 174, 42, 62, 134, 106}));
  return 0;
}
