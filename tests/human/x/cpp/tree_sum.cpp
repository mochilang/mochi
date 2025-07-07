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

int sum_tree(const Tree* t) {
    if (dynamic_cast<const Leaf*>(t)) {
        return 0;
    }
    auto n = static_cast<const Node*>(t);
    return sum_tree(n->left.get()) + n->value + sum_tree(n->right.get());
}

int main() {
    auto t = std::make_unique<Node>();
    t->left = std::make_unique<Leaf>();
    t->value = 1;
    t->right = std::make_unique<Node>();
    static_cast<Node*>(t->right.get())->left = std::make_unique<Leaf>();
    static_cast<Node*>(t->right.get())->value = 2;
    static_cast<Node*>(t->right.get())->right = std::make_unique<Leaf>();
    std::cout << sum_tree(t.get()) << std::endl;
    return 0;
}
