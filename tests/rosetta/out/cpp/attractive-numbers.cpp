// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:08Z
#include <iostream>
#include <string>

bool isPrime(int n) {
  if ((n < 2)) {
    return false;
  }
  if (((n % 2) == 0)) {
    return (n == 2);
  }
  if (((n % 3) == 0)) {
    return (n == 3);
  }
  auto d = 5;
  while (((d * d) <= n)) {
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 2);
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 4);
  }
  return true;
}

int countPrimeFactors(int n) {
  if ((n == 1)) {
    return 0;
  }
  if (isPrime(n)) {
    return 1;
  }
  auto count = 0;
  auto f = 2;
  while (true) {
    if (((n % f) == 0)) {
      count = (count + 1);
      n = (n / f);
      if ((n == 1)) {
        return count;
      }
      if (isPrime(n)) {
        f = n;
      }
    }
  }
  return count;
}

std::string pad4(int n) {
  auto s = std::to_string(n);
  while ((s.size() < 4)) {
    s = (std::string(" ") + s);
  }
  return s;
}

auto main() {
  auto max = 120;
  std::cout << ((std::string("The attractive numbers up to and including ") +
                 std::to_string(max)) +
                std::string(" are:"))
            << std::endl;
  auto count = 0;
  auto line = std::string("");
  auto lineCount = 0;
  auto i = 1;
  while ((i <= max)) {
    auto c = countPrimeFactors(i);
    if (isPrime(c)) {
      line = (line + pad4(i));
      count = (count + 1);
      lineCount = (lineCount + 1);
      if ((lineCount == 20)) {
        std::cout << line << std::endl;
        line = std::string("");
        lineCount = 0;
      }
    }
    i = (i + 1);
  }
  if ((lineCount > 0)) {
    std::cout << line << std::endl;
  }
}

int main() {
  main();
  return 0;
}
