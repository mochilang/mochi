#include <stdio.h>
#include <stdlib.h>

typedef struct Tree Tree;
typedef struct Leaf Leaf;
typedef struct Node Node;

typedef struct Leaf {
} Leaf;
typedef struct Node {
  Tree *left;
  int value;
  Tree *right;
} Node;
typedef struct Tree {
  int tag;
  union {
    Leaf Leaf;
    Node Node;
  } value;
} Tree;
#define Tree_Leaf 0
#define Tree_Node 1

int sum_tree(Tree t) {
  Tree _t1 = t;
  int _t2;
  switch (_t1.tag) {
  case Tree_Leaf:
    _t2 = 0;
    break;
  case Tree_Node:
    Tree left = *_t1.value.Node.left;
    int value = _t1.value.Node.value;
    Tree right = *_t1.value.Node.right;
    _t2 = sum_tree(left) + value + sum_tree(right);
    break;
  default:
    _t2 = 0;
    break;
  }
  return _t2;
}

int main() {
  __auto_type t =
      (Tree){.tag = Tree_Node,
             .value.Node = (Node){
                 .left = &(Tree){.tag = Tree_Leaf},
                 .value = 1,
                 .right = &(Tree){.tag = Tree_Node,
                                  .value.Node = (Node){
                                      .left = &(Tree){.tag = Tree_Leaf},
                                      .value = 2,
                                      .right = &(Tree){.tag = Tree_Leaf}}}}};
  printf("%d\n", sum_tree(t));
  return 0;
}
