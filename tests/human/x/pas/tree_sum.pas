program TreeSum;

type
  PTree = ^Tree;
  TreeKind = (tkLeaf, tkNode);
  Tree = record
    kind: TreeKind;
    left: PTree;
    value: integer;
    right: PTree;
  end;

function NewLeaf: PTree;
var
  t: PTree;
begin
  New(t);
  t^.kind := tkLeaf;
  t^.left := nil;
  t^.right := nil;
  t^.value := 0;
  NewLeaf := t;
end;

function NewNode(l: PTree; v: integer; r: PTree): PTree;
var
  t: PTree;
begin
  New(t);
  t^.kind := tkNode;
  t^.left := l;
  t^.value := v;
  t^.right := r;
  NewNode := t;
end;

function sum_tree(t: PTree): integer;
begin
  if t = nil then
    sum_tree := 0
  else if t^.kind = tkLeaf then
    sum_tree := 0
  else
    sum_tree := sum_tree(t^.left) + t^.value + sum_tree(t^.right);
end;

var
  t: PTree;
begin
  t := NewNode(NewLeaf, 1, NewNode(NewLeaf, 2, NewLeaf));
  Writeln(sum_tree(t));
end.
