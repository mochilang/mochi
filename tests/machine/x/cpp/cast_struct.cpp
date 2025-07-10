#include <iostream>

struct Todo {
  std::string title;
};
auto todo = Todo{.title = std::string("hi")};

int main() {
  {
    std::cout << std::boolalpha << todo.title;
    std::cout << std::endl;
  }
  return 0;
}
