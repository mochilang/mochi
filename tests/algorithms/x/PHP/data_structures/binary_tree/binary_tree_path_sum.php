<?php
ini_set('memory_limit', '-1');
function dfs($node, $target, $current) {
  return (function($__v) {
  if ($__v['__tag'] === "Empty") {
    return 0;
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $v = $__v["value"];
    $r = $__v["right"];
    return (($current + $v == $target ? 1 : 0)) + dfs($l, $target, $current + $v) + dfs($r, $target, $current + $v);
  }
})($node);
}
function path_sum($node, $target) {
  return (function($__v) {
  if ($__v['__tag'] === "Empty") {
    return 0;
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $v = $__v["value"];
    $r = $__v["right"];
    return dfs($node, $target, 0) + path_sum($l, $target) + path_sum($r, $target);
  }
})($node);
}
function sample_tree_one() {
  return ['__tag' => 'Node', 'left' => 10, 'value' => ['__tag' => 'Node', 'left' => 5, 'value' => ['__tag' => 'Node', 'left' => 3, 'value' => ['__tag' => 'Node', 'left' => 3, 'value' => $Empty, 'right' => $Empty], 'right' => ['__tag' => 'Node', 'left' => -2, 'value' => $Empty, 'right' => $Empty]], 'right' => ['__tag' => 'Node', 'left' => 2, 'value' => $Empty, 'right' => ['__tag' => 'Node', 'left' => 1, 'value' => $Empty, 'right' => $Empty]]], 'right' => ['__tag' => 'Node', 'left' => -3, 'value' => $Empty, 'right' => ['__tag' => 'Node', 'left' => 11, 'value' => $Empty, 'right' => $Empty]]];
}
function sample_tree_two() {
  return ['__tag' => 'Node', 'left' => 10, 'value' => ['__tag' => 'Node', 'left' => 5, 'value' => ['__tag' => 'Node', 'left' => 3, 'value' => ['__tag' => 'Node', 'left' => 3, 'value' => $Empty, 'right' => $Empty], 'right' => ['__tag' => 'Node', 'left' => -2, 'value' => $Empty, 'right' => $Empty]], 'right' => ['__tag' => 'Node', 'left' => 2, 'value' => $Empty, 'right' => ['__tag' => 'Node', 'left' => 1, 'value' => $Empty, 'right' => $Empty]]], 'right' => ['__tag' => 'Node', 'left' => -3, 'value' => $Empty, 'right' => ['__tag' => 'Node', 'left' => 10, 'value' => $Empty, 'right' => $Empty]]];
}
function main() {
  $tree1 = sample_tree_one();
  echo rtrim(json_encode(path_sum($tree1, 8), 1344)), PHP_EOL;
  echo rtrim(json_encode(path_sum($tree1, 7), 1344)), PHP_EOL;
  $tree2 = sample_tree_two();
  echo rtrim(json_encode(path_sum($tree2, 8), 1344)), PHP_EOL;
}
main();
