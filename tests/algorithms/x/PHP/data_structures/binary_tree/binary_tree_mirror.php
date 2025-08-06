<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function binary_tree_mirror_dict(&$tree, $root) {
  if (($root == 0) || (!(array_key_exists($root, $tree)))) {
  return;
}
  $children = $tree[$root];
  $left = $children[0];
  $right = $children[1];
  $tree[$root] = [$right, $left];
  binary_tree_mirror_dict($tree, $left);
  binary_tree_mirror_dict($tree, $right);
}
function binary_tree_mirror($binary_tree, $root) {
  if (count($binary_tree) == 0) {
  $panic('binary tree cannot be empty');
}
  if (!(array_key_exists($root, $binary_tree))) {
  $panic('root ' . _str($root) . ' is not present in the binary_tree');
}
  $tree_copy = [];
  foreach (array_keys($binary_tree) as $k) {
  $tree_copy[$k] = $binary_tree[$k];
};
  binary_tree_mirror_dict($tree_copy, $root);
  return $tree_copy;
}
function main() {
  $binary_tree = [1 => [2, 3], 2 => [4, 5], 3 => [6, 7], 7 => [8, 9]];
  echo rtrim('Binary tree: ' . _str($binary_tree)), PHP_EOL;
  $mirrored = binary_tree_mirror($binary_tree, 1);
  echo rtrim('Binary tree mirror: ' . _str($mirrored)), PHP_EOL;
}
main();
