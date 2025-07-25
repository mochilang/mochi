// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
#include <chrono>
#include <iostream>
#include <string>
#include <unordered_map>

inline long long _now() {
  auto n = std::chrono::system_clock::now().time_since_epoch();
  return std::chrono::duration_cast<std::chrono::nanoseconds>(n).count();
}
int parseIntStr(std::string str) {
  auto i = 0;
  auto neg = false;
  if (((str.size() > 0) &&
       (std::string(str).substr(0, (1) - (0)) == std::string("-")))) {
    neg = true;
    i = 1;
  }
  auto n = 0;
  auto digits = std::unordered_map<std::string, int>{
      {std::string("0"), 0}, {std::string("1"), 1}, {std::string("2"), 2},
      {std::string("3"), 3}, {std::string("4"), 4}, {std::string("5"), 5},
      {std::string("6"), 6}, {std::string("7"), 7}, {std::string("8"), 8},
      {std::string("9"), 9}};
  while ((i < str.size())) {
    n = ((n * 10) + digits[std::string(str).substr(i, ((i + 1)) - (i))]);
    i = (i + 1);
  }
  if (neg) {
    n = (-n);
  }
  return n;
}

auto __mochi_main() {
  auto total = 0;
  auto computer = ((_now() % 2) == 0);
  std::cout << std::string("Enter q to quit at any time\n") << std::endl;
  if (computer) {
    std::cout << std::string("The computer will choose first") << std::endl;
  } else {
    std::cout << std::string("You will choose first") << std::endl;
  }
  std::cout << std::string("\n\nRunning total is now 0\n\n") << std::endl;
  auto round = 1;
  auto done = false;
  while ((!done)) {
    std::cout << ((std::string("ROUND ") + std::to_string(round)) +
                  std::string(":\n\n"))
              << std::endl;
    auto i = 0;
    while (((i < 2) && ((!done)))) {
      if (computer) {
        auto choice = 0;
        if ((total < 18)) {
          choice = ((_now() % 3) + 1);
        } else {
          choice = (21 - total);
        }
        total = (total + choice);
        std::cout << (std::string("The computer chooses ") +
                      std::to_string(choice))
                  << std::endl;
        std::cout << (std::string("Running total is now ") +
                      std::to_string(total))
                  << std::endl;
        if ((total == 21)) {
          std::cout << std::string(
                           "\nSo, commiserations, the computer has won!")
                    << std::endl;
          done = true;
        }
      } else {
        while (true) {
          std::cout << std::string("Your choice 1 to 3 : ") << std::endl;
          auto line = input();
          if (((line == std::string("q")) || (line == std::string("Q")))) {
            std::cout << std::string("OK, quitting the game") << std::endl;
            done = true;
            break;
          }
          auto num = parseIntStr(line);
          if (((num < 1) || (num > 3))) {
            if (((total + num) > 21)) {
              std::cout << std::string("Too big, try again") << std::endl;
            } else {
              std::cout << std::string("Out of range, try again") << std::endl;
            }
            continue;
          }
          if (((total + num) > 21)) {
            std::cout << std::string("Too big, try again") << std::endl;
            continue;
          }
          total = (total + num);
          std::cout << (std::string("Running total is now ") +
                        std::to_string(total))
                    << std::endl;
          break;
        }
        if ((total == 21)) {
          std::cout << std::string("\nSo, congratulations, you've won!")
                    << std::endl;
          done = true;
        }
      }
      std::cout << std::string("\n") << std::endl;
      computer = (!computer);
      i = (i + 1);
    }
    round = (round + 1);
  }
}

int main() {
  __mochi_main();
  return 0;
}
