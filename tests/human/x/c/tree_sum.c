// tree_sum.c - manual translation of tests/vm/valid/tree_sum.mochi
#include <stdio.h>

typedef enum { LEAF, NODE } Tag;

typedef struct Tree {
    Tag tag;
    struct Tree *left;
    int value;
    struct Tree *right;
} Tree;

int sum_tree(const Tree *t) {
    if(t->tag == LEAF) return 0;
    return sum_tree(t->left) + t->value + sum_tree(t->right);
}

int main() {
    Tree leaf = {LEAF, NULL, 0, NULL};
    Tree nodeRight = {NODE, &leaf, 2, &leaf};
    Tree root = {NODE, &leaf, 1, &nodeRight};
    printf("%d\n", sum_tree(&root));
    return 0;
}
