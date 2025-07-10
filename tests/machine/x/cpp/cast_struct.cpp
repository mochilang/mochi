#include <iostream>

int main() {
  struct Todo {
    std::string title;
  };
  auto todo = Todo{.title = std::string("hi")};
  {
    std::cout << std::boolalpha << todo.title;
    std::cout << std::endl;
  }
  return 0;
}
