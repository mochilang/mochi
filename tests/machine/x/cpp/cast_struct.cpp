#include <iostream>
#include <string>

struct Todo {
  std::string title;
};
inline bool operator==(const Todo &a, const Todo &b) {
  return a.title == b.title;
}
inline bool operator!=(const Todo &a, const Todo &b) { return !(a == b); }
int main() {
  auto todo = Todo{.title = std::string("hi")};
  std::cout << std::boolalpha << todo.title << std::endl;
  return 0;
}
