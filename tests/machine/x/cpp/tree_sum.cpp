#include <algorithm>
#include <iostream>
#include <memory>

struct Tree {
  virtual ~Tree() = default;
};
struct Leaf : Tree {};
struct Node : Tree {
  std::unique_ptr<Tree> left;
  int value;
  std::unique_ptr<Tree> right;
};

int sum_tree(Tree *t) {
  return ([&]() {
    auto __v = t;
    if (dynamic_cast<Leaf *>(__v))
      return 0;
    if (auto __p1 = dynamic_cast<Node *>(__v)) {
      return ((sum_tree(__p1->left.get()) + __p1->value) +
              sum_tree(__p1->right.get()));
    }
    return decltype(0){};
  })();
}

int main() {
  auto t = ([&]() {
    auto __p = new Node();
    __p->left = std::unique_ptr<Tree>(new Leaf{});
    __p->value = 1;
    __p->right = std::unique_ptr<Tree>(([&]() {
      auto __p = new Node();
      __p->left = std::unique_ptr<Tree>(new Leaf{});
      __p->value = 2;
      __p->right = std::unique_ptr<Tree>(new Leaf{});
      return __p;
    })());
    return __p;
  })();
  std::cout << std::boolalpha << sum_tree(t) << std::endl;
  return 0;
}
