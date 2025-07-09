#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct Tree Tree;
typedef struct Leaf Leaf;
typedef struct Node Node;

typedef struct {
} Leaf;
typedef struct {
  Tree left;
  int value;
  Tree right;
} Node;
typedef struct {
  int tag;
  union {
    Leaf Leaf;
    Node Node;
  } value;
} Tree;

int sum_tree(Tree t) {
  return (t == Leaf ? 0
                    : (t == Node(left, value, right)
                           ? sum_tree(left) + value + sum_tree(right)
                           : 0));
}

int main() {
  int t = (Node){.left = Leaf,
                 .value = 1,
                 .right = (Node){.left = Leaf, .value = 2, .right = Leaf}};
  printf("%d\n", sum_tree(t));
  return 0;
}
