#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
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
                           ? ((sum_tree(left) + value) + sum_tree(right))
                           : 0));
}

int main() {
  int t = (Node){.left = Leaf,
                 .value = 1,
                 .right = (Node){.left = Leaf, .value = 2, .right = Leaf}};
  printf("%d\n", sum_tree(t));
  return 0;
}
