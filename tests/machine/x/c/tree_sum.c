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
  Tree tmp1 = t;
  int tmp2;
  switch (tmp1.tag) {
  case Tree_Leaf:
    tmp2 = 0;
    break;
  case Tree_Node:
    Tree left = *tmp1.value.Node.left;
    int value = tmp1.value.Node.value;
    Tree right = *tmp1.value.Node.right;
    tmp2 = sum_tree(left) + value + sum_tree(right);
    break;
  default:
    tmp2 = 0;
    break;
  }
  return tmp2;
}

int main() {
  Tree t =
      (Tree){.tag = Tree_Node,
             .value.Node = (Node){
                 .left = &(Tree){.tag = Tree_Leaf},
                 .value = 1,
                 .right = &(Tree){.tag = Tree_Node,
                                  .value.Node = (Node){
                                      .left = &(Tree){.tag = Tree_Leaf},
                                      .value = 2,
                                      .right = &(Tree){.tag = Tree_Leaf}}}}};
  printf("%.16g\n", sum_tree(t));
  return 0;
}
