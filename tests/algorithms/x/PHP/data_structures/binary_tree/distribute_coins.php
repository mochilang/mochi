<?php
ini_set('memory_limit', '-1');
function count_nodes($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  return count_nodes($nodes, $node['left']) + count_nodes($nodes, $node['right']) + 1;
}
function count_coins($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  return count_coins($nodes, $node['left']) + count_coins($nodes, $node['right']) + $node['data'];
}
$total_moves = 0;
function iabs($x) {
  global $total_moves;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function dfs($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  $left_excess = dfs($nodes, $node['left']);
  $right_excess = dfs($nodes, $node['right']);
  $abs_left = iabs($left_excess);
  $abs_right = iabs($right_excess);
  $total_moves = $total_moves + $abs_left + $abs_right;
  return $node['data'] + $left_excess + $right_excess - 1;
}
function distribute_coins($nodes, $root) {
  global $total_moves;
  if ($root == 0) {
  return 0;
}
  if (count_nodes($nodes, $root) != count_coins($nodes, $root)) {
  $panic('The nodes number should be same as the number of coins');
}
  $total_moves = 0;
  dfs($nodes, $root);
  return $total_moves;
}
function main() {
  global $total_moves;
  $example1 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 3, 'left' => 2, 'right' => 3], ['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 0, 'right' => 0]];
  $example2 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 2, 'right' => 3], ['data' => 3, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 0, 'right' => 0]];
  $example3 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 2, 'right' => 3], ['data' => 0, 'left' => 0, 'right' => 0], ['data' => 3, 'left' => 0, 'right' => 0]];
  echo rtrim(json_encode(distribute_coins($example1, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins($example2, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins($example3, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins([['data' => 0, 'left' => 0, 'right' => 0]], 0), 1344)), PHP_EOL;
}
main();
