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
function mirror_node(&$left, &$right, $idx) {
  if ($idx == (-1)) {
  return;
}
  $temp = $left[$idx];
  $left[$idx] = $right[$idx];
  $right[$idx] = $temp;
  mirror_node($left, $right, $left[$idx]);
  mirror_node($left, $right, $right[$idx]);
}
function mirror(&$tree) {
  mirror_node($tree['left'], $tree['right'], $tree['root']);
  return $tree;
}
function inorder($tree, $idx) {
  if ($idx == (-1)) {
  return [];
}
  $left_vals = inorder($tree, $tree['left'][$idx]);
  $right_vals = inorder($tree, $tree['right'][$idx]);
  return array_merge(array_merge($left_vals, [$tree[$values][$idx]]), $right_vals);
}
function make_tree_zero() {
  return ['values' => [0], 'left' => [-1], 'right' => [-1], 'root' => 0];
}
function make_tree_seven() {
  return ['values' => [1, 2, 3, 4, 5, 6, 7], 'left' => [1, 3, 5, -1, -1, -1, -1], 'right' => [2, 4, 6, -1, -1, -1, -1], 'root' => 0];
}
function make_tree_nine() {
  return ['values' => [1, 2, 3, 4, 5, 6, 7, 8, 9], 'left' => [1, 3, -1, 6, -1, -1, -1, -1, -1], 'right' => [2, 4, 5, 7, 8, -1, -1, -1, -1], 'root' => 0];
}
function main() {
  $names = ['zero', 'seven', 'nine'];
  $trees = [make_tree_zero(), make_tree_seven(), make_tree_nine()];
  $i = 0;
  while ($i < count($trees)) {
  $tree = $trees[$i];
  echo rtrim('      The ' . $names[$i] . ' tree: ' . _str(inorder($tree, $tree['root']))), PHP_EOL;
  $mirrored = mirror($tree);
  echo rtrim('Mirror of ' . $names[$i] . ' tree: ' . _str(inorder($mirrored, $mirrored['root']))), PHP_EOL;
  $i = $i + 1;
};
}
main();
