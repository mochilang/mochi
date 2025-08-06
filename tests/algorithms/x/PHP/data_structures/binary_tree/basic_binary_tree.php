<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function inorder($nodes, $index, $acc) {
  global $small, $medium;
  if ($index == 0 - 1) {
  return $acc;
}
  $node = $nodes[$index];
  $res = inorder($nodes, $node['left'], $acc);
  $res = _append($res, $node['data']);
  $res = inorder($nodes, $node['right'], $res);
  return $res;
}
function size($nodes, $index) {
  global $small, $medium;
  if ($index == 0 - 1) {
  return 0;
}
  $node = $nodes[$index];
  return 1 + size($nodes, $node['left']) + size($nodes, $node['right']);
}
function depth($nodes, $index) {
  global $small, $medium;
  if ($index == 0 - 1) {
  return 0;
}
  $node = $nodes[$index];
  $left_depth = depth($nodes, $node['left']);
  $right_depth = depth($nodes, $node['right']);
  if ($left_depth > $right_depth) {
  return $left_depth + 1;
}
  return $right_depth + 1;
}
function is_full($nodes, $index) {
  global $small, $medium;
  if ($index == 0 - 1) {
  return true;
}
  $node = $nodes[$index];
  if ($node['left'] == 0 - 1 && $node['right'] == 0 - 1) {
  return true;
}
  if ($node['left'] != 0 - 1 && $node['right'] != 0 - 1) {
  return is_full($nodes, $node['left']) && is_full($nodes, $node['right']);
}
  return false;
}
function small_tree() {
  global $small, $medium;
  $arr = [];
  $arr = _append($arr, ['data' => 2, 'left' => 1, 'right' => 2]);
  $arr = _append($arr, ['data' => 1, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 3, 'left' => 0 - 1, 'right' => 0 - 1]);
  return $arr;
}
function medium_tree() {
  global $small, $medium;
  $arr = [];
  $arr = _append($arr, ['data' => 4, 'left' => 1, 'right' => 4]);
  $arr = _append($arr, ['data' => 2, 'left' => 2, 'right' => 3]);
  $arr = _append($arr, ['data' => 1, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 3, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 5, 'left' => 0 - 1, 'right' => 5]);
  $arr = _append($arr, ['data' => 6, 'left' => 0 - 1, 'right' => 6]);
  $arr = _append($arr, ['data' => 7, 'left' => 0 - 1, 'right' => 0 - 1]);
  return $arr;
}
$small = small_tree();
echo rtrim(json_encode(size($small, 0), 1344)), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inorder($small, 0, []), 1344))))))), PHP_EOL;
echo rtrim(json_encode(depth($small, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(is_full($small, 0), 1344)), PHP_EOL;
$medium = medium_tree();
echo rtrim(json_encode(size($medium, 0), 1344)), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inorder($medium, 0, []), 1344))))))), PHP_EOL;
echo rtrim(json_encode(depth($medium, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(is_full($medium, 0), 1344)), PHP_EOL;
