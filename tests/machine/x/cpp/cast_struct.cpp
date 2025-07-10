#include <iostream>

struct Todo {
  std::string title;
};
inline bool operator==(const Todo &a, const Todo &b) {
  return a.title == b.title;
}
inline bool operator!=(const Todo &a, const Todo &b) { return !(a == b); }
auto todo = Todo{.title = std::string("hi")};

int main() {
  std::cout << std::boolalpha << todo.title << std::endl;
  return 0;
}
